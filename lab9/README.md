# Lab 09: REST APIs

* Deadline (soft): Friday, 28th of March, 16:00
* Deadline (hard): 11th of April, 16:00
* Score: 4 
* Language: Rust or Haskell
* ChatGPT: **use it**. Ask ChatGPT for explanations and for code.
* Co-pilot: **use it**. Get help from ChatGPT implementing the core functionality.


# Haskell and Rust

This lab is designed to be done as "points scoring lab for both, Rust and Haskell".
Thus, represents actually two identical labs with a different programming language. 
Those that want to include it into their portfolio are encouraged to (re-)implement
it in both languages and discuss the pros and cons of 




# Hello JSON 

Build a simple webserver serving some simple API points, and handling JSON input/output.

## The specs 

1. The web server should be able to handle HTTP GET and POST requests.
2. Implement a basic routing mechanism to serve different endpoints with appropriate responses:
   * `GET /hello` returns string: "Hello, World!" and 200 OK
   * `GET /` returns 404 page not found
   * `GET /greet/<name>`, for example `GET /greet/Mariusz` returns JSON `{ "greet": "Hello", "name": "Mariusz"}` and `200 OK`
      * Note, the URL path above is only an example, the last element of the URL path is a parameter, that will be used a the name.
      * Your code should work for "/greet/Adam" (or whatever different name) and return appropriately, based on the parameter passed into it.
   * POST "/greetme" with JSON input `{ "input": "whatever text", "name": "Mariusz"}` returns JSON `{"msg": "whatever text Mariusz"}`
4. Ensure proper error handling for various scenarios, such as invalid requests or server errors.


# Task Check Request queue

## The specs

The exact specs details are up to you to, but, the project needs to have the following functionality:

* Build a simple front-end to our course that serves either HTML page or JSON struct with the list of students currently in the queue for lab check-ins. The choice between serving HTML or JSON can be done as a parameter to a single GET endpoint, or as two endpoints. But you need to serve HTML and JSON lists of students in the queue.
* To obtain the open issues in the course use the following GET query:
`https://git.gvk.idi.ntnu.no/api/v4/projects/5881/issues?labels=Task%20Check%20Request&state=opened`
* It will respond with a JSON with all the opened issues for "Task Check Request". Sort them into a list from earliest to the latest, and provide username and real names of students in the queue. 


# Rust

**References:**
1. "The Rust Programming Language" book, particularly chapters on concurrency and error handling.
2. Rust documentation and official crates like `axum` for building web servers. See below.
3. Online tutorials and examples for building concurrent web servers in Rust.
4. Academic papers on concurrent programming and web server design for additional insights.


## External crates to use

Use the following crates in your implementation:

0. Use **axum** crate for building the web server. Axum is a thin layer on top of 
low-level networking API for HTTP, based on **hyper**. 

1. **tokio:** This crate provides asynchronous I/O primitives, scheduler, and runtime for building highly concurrent applications in Rust. It's commonly used in conjunction with `hyper` or `axum` for implementing asynchronous web servers.

2. **serde:** This crate is essential for serializing and deserializing data structures in Rust. Students can use `serde` to parse incoming HTTP requests and generate appropriate responses.

3. **serde_json:** You will need to work with JSON data. Use this crate for serializing and deserializing JSON in Rust.


## External crates NOT to be used

In general, avoid any additional crates beyond the ones specified above. Specifically, if ChatGPT
offers you solutions based on `hyper` directly, you should enforce it to use `axum` instead.
Solutions with other random crates will not be accepted.

* **hyper:** This is low-level api crate and you will use it for high-performance web server or client,
  but, for this exercise we DO NOT use hyper directly.
* **log and env_logger:** These crates provide logging utilities for Rust applications. 
  Implementing logging in the web server can help you debug issues and monitor its behavior.
  However, for this lab do not complicate your implementation with logging and do not use logging.

* **anyhow:** This crate offers convenient error handling utilities in Rust, making it 
  easier to propagate and handle errors throughout the application. It simplifies error 
  handling compared to using Rust's built-in `Result` and `Option` types directly.
  However, for this lab, use only the built-in facilities of Rust. 

* **dotenv:** This crate allows loading environment variables from a `.env` file,
  which can be useful for configuring the web server without hardcoding values
  directly into the code. However, for this lab, do not use .env file.
  Use hardcoded constants in the code instead.

* **futures:** Although `tokio` provides its own implementation of futures,
  the `futures` crate offers additional utilities and combinators for working
  with asynchronous programming in Rust. This could offer some benefits, but
  for this lab, stick to `tokio` only.



# Haskell 

## External dependencies to use

* servant
* servant-server
* aeson
* warp

For JSON parsing and for web server end-points do not use any other libraries than those listed above. 
Feel free to use any additional dependencies if that will make your implementation better. Motivate your choices. 