# Servant Persistant Example CRUD app

uses servant for developing api and Persistant for ORM

## Setup
- make sure you have docker installed
- just run `docker compose build` and `docker compose up`

## Api Endpoints

### User
- create new user account
```bash
curl --verbose
    -X POST
    -H "Content-Type: application/json"
    -d '{"name": "dhruv sharma", "email": "dhruv@gmail.com", "password": "pass"}'
    http://localhost:8080/user
```
- get user details
```bash
curl --verbose 
    -X GET 
    -H "Content-Type: application/json" 
    -H "Authorization: Basic $(printf 'dhruv@gmail.com:pass' | base64)" 
    http://localhost:8080/user/51
```
- update user details
```bash
curl --verbose 
    -X PATCH 
    -H "Content-Type: application/json" 
    -H "Authorization: Basic $(printf 'dhruv1@gmail.com:pass' | base64)" 
    -d '{"name": "dhruvvvv"}' 
    http://localhost:8080/user/51
```
- delete user
```bash
curl --verbose 
    -X DELETE 
    -H "Content-Type: application/json" 
    -H "Authorization: Basic $(printf 'dhruv@gmail.com:pass' | base64)" 
    http://localhost:8080/user/51
```


### Movie
- add new movie
```bash
curl --verbose 
    -X POST 
    -H "Content-Type: application/json" 
    -H "Authorization: Basic $(printf 'dhruv1@gmail.com:pass' | base64)"
    -d '{"name": "my movie", "rating": 10}' 
    http://localhost:8080/movie
```
- get movie details
```bash
curl --verbose 
    -X GET  
    http://localhost:8080/movie/438
```
- update movie details
```bash
curl --verbose 
    -X PUT 
    -H "Content-Type: application/json" 
    -H "Authorization: Basic $(printf 'dhruv1@gmail.com:pass' | base64)" 
    -d '{"name": "my movie", "rating": 10}' 
    http://localhost:8080/movie/438
```
- delete movie
```bash
curl --verbose 
    -X DELETE
    -H "Authorization: Basic $(printf 'dhruv1@gmail.com:pass' | base64)" 
    http://localhost:8080/movie/438
```
- list all movies
```bash
curl --verbose 
    -X GET 
    http://localhost:8080/movies
```
- add movie to favourite
```bash
curl --verbose 
    -X POST 
    -H "Authorization: Basic $(printf 'dhruv1@gmail.com:pass' | base64)" 
    http://localhost:8080/fav/438
```
- remove movie from favourite
```bash
curl --verbose 
    -X DELETE 
    -H "Authorization: Basic $(printf 'dhruv1@gmail.com:pass' | base64)" 
    http://localhost:8080/fav/438
```
