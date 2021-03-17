/// web frontend hosting ulang

use actix_web::{get, post, web, App, HttpResponse, HttpServer, Responder};
use std::io;


#[get("/")]
async fn index() -> &'static str {
   " <h1>hello, ulang</h1>"
}

#[post("/health")]
async fn health(req_body: String) -> impl Responder {
    HttpResponse::Ok().body(req_body)
}

#[post("/compile")]
async fn compile(req_body: String) -> impl Responder {
    HttpResponse::Ok().body(req_body)
}

#[actix_web::main]
async fn main() -> io::Result<()> {
    println!("hello");
    HttpServer::new(|| {
        App::new()
            .service(index)
            .service(health)
    })
    .bind("127.0.0.1:8080")?
    .run()
    .await
}

