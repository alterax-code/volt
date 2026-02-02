use axum::{extract::Json, http::header, response::{Html, IntoResponse}, routing::{get, post}, Router};
use serde::{Deserialize, Serialize};
use std::{process::Command, time::Instant};

#[derive(Deserialize)]
struct RunReq {
    code: String,
}

#[derive(Serialize)]
struct RunResp {
    output: String,
    error: String,
    time_ms: f64,
    success: bool,
    details: String,
}

fn find_volt() -> Option<String> {
    if let Ok(b) = std::env::var("VOLT_BIN") {
        return Some(b);
    }
    for p in [
        "../target/release/volt.exe",
        "../target/release/volt",
        "../target/debug/volt.exe",
        "../target/debug/volt",
    ] {
        if std::path::Path::new(p).exists() {
            return Some(p.to_string());
        }
    }
    None
}

fn execute(code: &str) -> RunResp {
    let bin = match find_volt() {
        Some(b) => b,
        None => return RunResp {
            output: String::new(),
            error: "VOLT binary not found. Run: cargo build --release (in volt root)".into(),
            time_ms: 0.0,
            success: false,
            details: String::new(),
        },
    };

    let tmp = std::env::temp_dir().join(format!("volt_web_{}.volt", std::process::id()));
    if let Err(e) = std::fs::write(&tmp, code) {
        return RunResp {
            output: String::new(),
            error: format!("Write error: {e}"),
            time_ms: 0.0,
            success: false,
            details: String::new(),
        };
    }

    let start = Instant::now();
    let res = Command::new(&bin).args(["run", tmp.to_str().unwrap()]).output();
    let elapsed = start.elapsed();
    let _ = std::fs::remove_file(&tmp);

    match res {
        Ok(o) => {
            let stdout = String::from_utf8_lossy(&o.stdout).to_string();
            let stderr = String::from_utf8_lossy(&o.stderr).to_string();
            if o.status.success() {
                RunResp {
                    output: stdout,
                    error: String::new(),
                    time_ms: parse_ms(&stderr).unwrap_or(elapsed.as_secs_f64() * 1000.0),
                    success: true,
                    details: parse_details(&stderr),
                }
            } else {
                let err = stderr.lines()
                    .find(|l| !l.trim().is_empty())
                    .unwrap_or("Unknown error")
                    .to_string();
                RunResp {
                    output: stdout,
                    error: err,
                    time_ms: elapsed.as_secs_f64() * 1000.0,
                    success: false,
                    details: String::new(),
                }
            }
        }
        Err(e) => RunResp {
            output: String::new(),
            error: format!("Exec error: {e}"),
            time_ms: 0.0,
            success: false,
            details: String::new(),
        },
    }
}

fn parse_ms(s: &str) -> Option<f64> {
    s.lines()
        .find(|l| l.contains("Total:"))
        .and_then(|l| l.split("Total:").nth(1)?.trim().trim_end_matches("ms").trim().parse().ok())
}

fn parse_details(s: &str) -> String {
    s.lines()
        .filter(|l| {
            l.contains("Lexer:") || l.contains("Parser:")
                || l.contains("Compiler:") || l.contains("VM:")
        })
        .map(|l| l.trim())
        .collect::<Vec<_>>()
        .join("  |  ")
}

async fn run_code(Json(req): Json<RunReq>) -> Json<RunResp> {
    let code = req.code;
    let resp = tokio::time::timeout(
        std::time::Duration::from_secs(10),
        tokio::task::spawn_blocking(move || execute(&code)),
    )
    .await;
    Json(match resp {
        Ok(Ok(r)) => r,
        Ok(Err(e)) => RunResp {
            output: String::new(),
            error: format!("{e}"),
            time_ms: 0.0,
            success: false,
            details: String::new(),
        },
        Err(_) => RunResp {
            output: String::new(),
            error: "Timeout: execution exceeded 10 seconds".into(),
            time_ms: 10000.0,
            success: false,
            details: String::new(),
        },
    })
}

async fn index() -> impl IntoResponse {
    (
        [(header::CONTENT_TYPE, "text/html; charset=utf-8")],
        include_str!("../static/index.html"),
    )
}

#[tokio::main]
async fn main() {
    let port: u16 = std::env::var("PORT")
        .ok()
        .and_then(|p| p.parse().ok())
        .unwrap_or(3000);

    println!();
    println!("  VOLT Playground");
    println!("  http://localhost:{port}");
    println!();

    let app = Router::new()
        .route("/", get(index))
        .route("/api/run", post(run_code));

    let listener = tokio::net::TcpListener::bind(format!("0.0.0.0:{port}"))
        .await
        .unwrap();
    axum::serve(listener, app).await.unwrap();
}