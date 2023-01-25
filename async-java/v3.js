// Video 3
console.log("Before");
getUser(1, (user) => {
    console.log("User ", user);
    getRepositories(user.githubUsername, (repo) => {
        console.log("Repositories ", repo);
    });
});

console.log("After");
function getUser(id, callback)
{
    setTimeout(() => {
        console.log("Reading a user from database..");
        callback({id: id, githubUsername: "moneebullah25"});
    }, 2000);
    // will be printed after the "Before" log command
    console.log("Outside the timeout function"); 
}

// Exercise
function getRepositories(username, callback)
{
    setTimeout(() => {
        callback(['repo 1', 'repo 2', 'repo 3']);
    }, 2000);
}
