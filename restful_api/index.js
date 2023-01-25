const joi = require('joi');
const express = require("express");
const app = express();

app.use(express.json()); // enable req.body.name

function validateCourse(course) {
    const schema = {name: joi.string().min(3).required};
    return joi.validate(req.body, schema);
}

const courses = [
    {id: 1, name: "Course 1"},
    {id: 2, name: "Course 2"},
    {id: 3, name: "Course 3"}
];

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
    const course = {id: courses.length, name: req.body.name };
    course.push(course);
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
    course.splice(index, 1);

    res.send(course);
})


app.listen(3000, () => console.log("Listening on port 3000"));