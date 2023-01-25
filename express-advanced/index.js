const startupDebugger = require("debug")("app:startup");
const dbDebugger = require("debug")("app:db");

// const Joi = require('joi');
const express = require("express");
const app = express();
const cusMiddleWare = require("./logger");
const morgan = require('morgan');


// /* process.env.NODE_ENV is undefined by defualt */
// console.log(`NODE ENV: ${process.env.NODE_ENV}`); 
// /* app.get('env') is developement by default */
// console.log(`app.get: ${app.get('env')}`);

if (app.get('env') === 'development') 
{// Middleware function slowed the response time
    app.use(morgan("tiny")); 
    startupDebugger("morgan enabled");
}

dbDebugger("Conected to Database");

/* Middlewares
    Either return the response to the client on request 
    OR passes control to another middleware function */
// if body exists parse body to json object
app.use(express.json()); // enable req.body.name, middleware func 
/* Middleware function are called in sequence
    And each custom middleware should be stored in another file */
app.use(cusMiddleWare.logger);
app.use(cusMiddleWare.authenticate);

function validateCourse(course) {
    let result = {
        error: {
            details: [{
                    message: ""
                }
            ]
        }
    };
    if (!course.name) {
        result.error.details[0].message = "name is required";
        return result;
    }
    else if (course.name.length < 3){
        result.error.details[0].message = "name should be more than 3 characters";
        return result;
    }
    else {
        result.error = null;
        return result;
    }
}

const courses = [
    {id: 1, name: "Course 1"},
    {id: 2, name: "Course 2"},
    {id: 3, name: "Course 3"}
];

/* Route Handler
    all app.get, app.post, app.put, app.delete are the route handler
    than return an object if request is recieved */
app.get('/', (req, res) => {
    res.send("Hello World");
});

app.get("/api/courses/", (req, res) => {
    res.send(courses);
});

app.get("/api/courses/:id", (req, res) => {
    const course = courses.find(c => c.id === parseInt(req.params.id));
    // console.log(req.query);
    let outputMsg = "Couldn't found the course with ID "+req.params.id+" "+req.query.sortBy;
    if (!course) res.status(404).send(outputMsg);
});

app.post("/api/courses/", (req, res) => {
    const {error} = validateCourse(req.body);
    if (error) {res.status(400).send(error.details[0].message);return;}

    const course = {id: courses.length + 1, name: req.body.name };
    courses.push(course);
    res.send(course);
});

// Just update the existing course
app.put("/api/courses/:id", (req, res) => {
    const course = courses.find(c => c.id === parseInt(req.params.id));
    let outputMsg = "Couldn't found the course with ID "+req.params.id+" "+req.query.sortBy;
    if (!course) res.status(404).send(outputMsg);

    const {error} = validateCourse(req.body);
    if (error) {res.status(400).send(error.details[0].message);return;}

    course.name = req.body.name;
    res.send(course);
});

app.delete("/api/courses/:id", (req, res) => {
    const course = courses.find(c => c.id === parseInt(req.params.id));
    let outputMsg = "Couldn't found the course with ID "+req.params.id+" "+req.query.sortBy;
    if (!course) res.status(404).send(outputMsg);

    const index = courses.indexOf(course);
    courses.splice(index, 1);

    res.send(course);
});


app.listen(3000, () => console.log("Listening on port 3000"));