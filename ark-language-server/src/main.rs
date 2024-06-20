use crossbeam::channel::{Receiver, Sender};
use crossbeam::select;
use lsp_server::{Connection, Message, Notification};
use lsp_types::notification::{self, Notification as _};
use lsp_types::request::{self, Request};
use lsp_types::{
    ClientCapabilities, Diagnostic, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, HoverProviderCapability,
    InitializeParams, OneOf, Position, PublishDiagnosticsParams, Range, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};
use parser::{ast, compute_line_column, compute_line_starts, ParseErrorWithLocation, Parser};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use symbols::document_hover_request;
use threadpool::ThreadPool;
use walkdir::WalkDir;

use crate::symbols::document_symbol_request;

mod symbols;

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    env_logger::init();

    let (connection, io_threads) = Connection::stdio();
    log::info!("start ark language server...");

    // Run the server
    let (id, params) = connection.initialize_start()?;

    let init_params: InitializeParams = serde_json::from_value(params).unwrap();
    let client_capabilities: ClientCapabilities = init_params.capabilities;
    let client_workspace_folders = init_params.workspace_folders.unwrap_or_default();
    let mut workspace_folders = Vec::new();

    for workspace_folder in client_workspace_folders {
        let file_path = workspace_folder
            .uri
            .to_file_path()
            .expect("file path expected");
        assert!(file_path.is_absolute());
        eprintln!("workspace folder: {}", file_path.display());
        workspace_folders.push(file_path);
    }

    let hover_provider = Some(HoverProviderCapability::Simple(true));

    let server_capabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        hover_provider,
        document_symbol_provider: Some(OneOf::Left(true)),
        ..Default::default()
    };

    let initialize_data = serde_json::json!({
        "capabilities": server_capabilities,
        "serverInfo": {
            "name": "ark-language-server",
            "version": "0.0.2"
        }
    });

    let projects = find_projects(&workspace_folders);
    for project in &projects {
        eprintln!("project {} -> {}", project.name, project.main.display());
    }

    connection.initialize_finish(id, initialize_data)?;
    let mut state = ServerState::new(client_capabilities, workspace_folders, projects);
    event_loop(&mut state, &connection)?;
    io_threads.join()?;
    Ok(())
}

struct ServerState {
    opened_files: HashMap<PathBuf, Arc<String>>,
    parsed_files: HashMap<PathBuf, Arc<ast::File>>,

    #[allow(dead_code)]
    client_capabilities: ClientCapabilities,
    #[allow(dead_code)]
    workspace_folders: Vec<PathBuf>,
    projects: Vec<ProjectConfig>,
    files_with_errors: HashSet<PathBuf>,

    threadpool: ThreadPool,
    threadpool_sender: Sender<MainLoopTask>,
    threadpool_receiver: Receiver<MainLoopTask>,
}

impl ServerState {
    fn new(
        client_capabilities: ClientCapabilities,
        workspace_folders: Vec<PathBuf>,
        projects: Vec<ProjectConfig>,
    ) -> ServerState {
        let threadpool = ThreadPool::new(6);
        let (sender, receiver) = crossbeam::channel::unbounded();

        ServerState {
            opened_files: HashMap::new(),
            parsed_files: HashMap::new(),
            client_capabilities,
            workspace_folders,
            projects,
            files_with_errors: HashSet::new(),
            threadpool,
            threadpool_sender: sender,
            threadpool_receiver: receiver,
        }
    }
}

fn event_loop(
    state: &mut ServerState,
    connection: &Connection,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let mut event;

    loop {
        event = next_event(state, connection);
        if event.is_none() {
            return Ok(());
        }

        match event {
            Some(Event::LanguageServer(msg)) => handle_message(state, msg),
            Some(Event::MainLoopTask(task)) => handle_main_loop_task(state, connection, task),
            None => {
                return Ok(());
            }
        }
    }
}

fn next_event(state: &mut ServerState, connection: &Connection) -> Option<Event> {
    select! {
        recv(connection.receiver) -> msg => {
            match msg {
                Ok(msg) => {
                    if let Message::Notification(ref notification) = msg {
                        if notification.method == notification::Exit::METHOD {
                            return None;
                        }
                    }

                    return Some(Event::LanguageServer(msg));
                }

                Err(error) => {
                    eprintln!("error from lsp connection: {}.", error);
                    return None;
                }
            }
        }

        recv(state.threadpool_receiver) -> msg => {
            match msg {
                Ok(msg) => Some(Event::MainLoopTask(msg)),
                Err(error) => {
                    eprintln!("error from thread pool: {}.", error);
                    None
                }
            }
        }
    }
}

fn handle_main_loop_task(state: &mut ServerState, connection: &Connection, task: MainLoopTask) {
    match task {
        MainLoopTask::ParsedFile(path, content, file) => {
            state.opened_files.insert(path.clone(), content);
            state.parsed_files.insert(path, file);
        },
        MainLoopTask::SendResponse(msg) => connection.sender.send(msg).expect("send failed"),
        MainLoopTask::ReportError(errors_by_file) => {
            eprintln!("{:?}", errors_by_file);

            let mut last_files_with_errors =
                std::mem::replace(&mut state.files_with_errors, HashSet::new());

            for (file, errors) in errors_by_file {
                let params = PublishDiagnosticsParams {
                    uri: Url::from_file_path(&file).expect("broken file path"),
                    version: None,
                    diagnostics: errors,
                };

                let notification =
                    Notification::new("textDocument/publishDiagnostics".into(), params);

                connection
                    .sender
                    .send(Message::Notification(notification))
                    .expect("send() failed");

                last_files_with_errors.remove(&file);
                state.files_with_errors.insert(file);
            }

            for file in last_files_with_errors {
                let params = PublishDiagnosticsParams {
                    uri: Url::from_file_path(&file).expect("broken file path"),
                    version: None,
                    diagnostics: Vec::new(),
                };

                let notification =
                    Notification::new("textDocument/publishDiagnostics".into(), params);

                connection
                    .sender
                    .send(Message::Notification(notification))
                    .expect("send() failed");
            }
        }
    }
}

fn handle_message(state: &mut ServerState, msg: Message) {
    match msg {
        Message::Notification(notification) => {
            eprintln!("received notification {}", notification.method);
            if notification.method == notification::DidChangeTextDocument::METHOD {
                did_change_notification(state, notification);
            } else if notification.method == notification::DidOpenTextDocument::METHOD {
                did_open_notification(state, notification);
            } else if notification.method == notification::DidCloseTextDocument::METHOD {
                did_close_notification(state, notification);
            } else if notification.method == notification::DidSaveTextDocument::METHOD {
                did_save_notification(state, notification);
            } else {
                eprintln!("unknown notification {}", notification.method);
                eprintln!("{}", notification.params);
            }
        }

        Message::Request(request) => {
            eprintln!("received request {}", request.method);

            if request.method == request::DocumentSymbolRequest::METHOD {
                document_symbol_request(state, request);
            } else if request.method == request::HoverRequest::METHOD {
                document_hover_request(state, request);
            } else if request.method == "$/cancelRequest" {
                eprintln!("got cancelRequest");
            } else {
                eprintln!("unknown request {}", request.method);
                eprintln!("{}", request.params);
            }
        }

        Message::Response(response) => eprintln!("unknown response {:?}", response.result),
    }
}

fn parse_ast(sender: Sender<MainLoopTask>, path: PathBuf, content: Arc<String>) {
    let line_starts = compute_line_starts(&content);
    let parser = Parser::from_shared_string(content.clone());
    let (file, parse_errors) = parser.parse();

    let mut diag = vec![];

    for error in parse_errors {
        let (line, column) = compute_line_column(&line_starts, error.span.start());
        let start = Position::new(line - 1, column - 1);
        let (line, column) = compute_line_column(&line_starts, error.span.end());
        let end = Position::new(line - 1, column - 1);

        diag.push(Diagnostic {
            range: Range::new(start, end),
            message: error.error.message(),
            ..Default::default()
        });
    }

    let mut errors_by_file: HashMap<PathBuf, Vec<Diagnostic>> = HashMap::new();
    errors_by_file.entry(path.clone()).or_default().append(&mut diag);

    sender
        .send(MainLoopTask::ParsedFile(path, content, file))
        .expect("send failed");

    sender
        .send(MainLoopTask::ReportError(errors_by_file))
        .expect("send failed");
}

fn did_change_notification(state: &mut ServerState, notification: Notification) {
    let result = serde_json::from_value::<DidChangeTextDocumentParams>(notification.params);

    match result {
        Ok(result) => {
            let path = result
                .text_document
                .uri
                .to_file_path()
                .expect("file path expected");
            let content = Arc::new(result.content_changes[0].text.clone());
            let sender = state.threadpool_sender.clone();

            state.threadpool.execute(move || {
                parse_ast(sender, path, content);
            });
        }

        Err(_) => {
            eprintln!("broken json");
        }
    }
}

fn did_open_notification(state: &mut ServerState, notification: Notification) {
    let result = serde_json::from_value::<DidOpenTextDocumentParams>(notification.params);
    match result {
        Ok(result) => {
            let path = result
                .text_document
                .uri
                .to_file_path()
                .expect("file path expected");
            let content = Arc::new(result.text_document.text);
            let sender = state.threadpool_sender.clone();

            state.threadpool.execute(move || {
                parse_ast(sender, path, content);
            });
        }
        Err(_) => {}
    }
}

fn did_close_notification(state: &mut ServerState, notification: Notification) {
    let result = serde_json::from_value::<DidCloseTextDocumentParams>(notification.params);
    match result {
        Ok(result) => {
            let path = result
                .text_document
                .uri
                .to_file_path()
                .expect("file path expected");
            state.opened_files.remove(&path);
        }
        Err(_) => {}
    }
}

fn did_save_notification(state: &mut ServerState, notification: Notification) {
    let result = serde_json::from_value::<DidSaveTextDocumentParams>(notification.params);

    match result {
        Ok(_) => {
            let sender = state.threadpool_sender.clone();
            let projects = state.projects.clone();

            state.threadpool.execute(move || {
                for project in projects {
                    compile_project(project, sender.clone());
                }
            })
        }
        Err(_) => {}
    }
}

fn compile_project(project: ProjectConfig, sender: Sender<MainLoopTask>) {
    log::info!("recompiling project");

    use arkc_frontend::sema::{Sema, SemaArgs};
    let sem_args = SemaArgs {
        arg_file: Some(project.main.to_string_lossy().into_owned()),
        packages: Vec::new(),
        test_file_as_string: None,
    };

    let mut sa = Sema::new(sem_args);

    let success = arkc_frontend::check_program(&mut sa);
    assert_eq!(success, !sa.diag.borrow().has_errors());
    let mut errors_by_file: HashMap<PathBuf, Vec<Diagnostic>> = HashMap::new();

    for error in sa.diag.borrow().errors() {
        eprintln!("error: {:?}", error);
        if let Some(file_id) = error.file_id {
            let span = error.span.expect("missing location");
            let source_file = sa.file(file_id);
            let line_starts = &source_file.line_starts;

            let (line, column) = compute_line_column(&line_starts, span.start());
            let start = Position::new(line - 1, column - 1);
            let (line, column) = compute_line_column(&line_starts, span.end());
            let end = Position::new(line - 1, column - 1);

            errors_by_file
                .entry(source_file.path.clone())
                .or_default()
                .push(Diagnostic {
                    range: Range::new(start, end),
                    message: error.msg.message(),
                    ..Default::default()
                })
        } else {
            unimplemented!()
        }
    }

    sender
        .send(MainLoopTask::ReportError(errors_by_file))
        .expect("failed send");
}

fn find_projects(workspaces: &[PathBuf]) -> Vec<ProjectConfig> {
    let mut projects = Vec::new();

    for workspace in workspaces {
        for entry in WalkDir::new(workspace) {
            let entry = entry.unwrap();
            if entry.file_name() == "ark-project.json" {
                let config = read_project_json(entry.path());

                match config {
                    Ok(config) => {
                        let path = entry.path().parent().expect("no parents");
                        let name = config.name;
                        let main_file = path.join(&config.main);
                        projects.push(ProjectConfig {
                            name,
                            main: main_file,
                        });
                    }

                    Err(_) => {
                        eprintln!("invalid project config at {}", entry.path().display());
                    }
                }
            }
        }
    }

    projects
}

fn read_project_json(path: &Path) -> Result<ProjectJsonConfig, Box<dyn Error>> {
    let mut file = File::open(path)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;

    let value: serde_json::Value = serde_json::from_str(&content)?;
    let parsed_value = serde_json::from_value::<ProjectJsonConfig>(value)?;

    Ok(parsed_value)
}

enum MainLoopTask {
    ParsedFile(PathBuf, Arc<String>, Arc<ast::File>),
    SendResponse(Message),
    ReportError(HashMap<PathBuf, Vec<Diagnostic>>),
}

enum Event {
    LanguageServer(Message),
    MainLoopTask(MainLoopTask),
}

#[derive(Clone)]
struct ProjectConfig {
    name: String,
    main: PathBuf,
}

#[derive(Serialize, Deserialize)]
struct ProjectJsonConfig {
    name: String,
    main: String,
    packages: Vec<String>,
}
