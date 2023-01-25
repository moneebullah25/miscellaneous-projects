const mongoose = require("mongoose");

mongoose.set("strictQuery", true);

mongoose.connect("mongodb://localhost:27017/playground")
    .then(() => console.log("Connected to MongoDB"))
    .catch(err => console.error("Couldn't connect to MongoDB... ", err));