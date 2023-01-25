// Video 8 Consuming Promises
// console.log("Before");
// getUser(1, (user) => {
//     console.log("User ", user);
//     getRepositories(user.githubUsername, (repo) => {
//         console.log("Repositories ", repo);
//         getCommits(repo[0]);
//     });
// });

/* Causing some error */
/* const p = getUser(1);
p.then((user) => {
    console.log("User", user);
    getRepositories(user.githubUsername);
}).then((repos) => {
    console.log("Repository", repos);
    getCommits(repos[0]);
}).then((commits) => {
    console.log("Commits", commits);
}); */

console.log("Begin");
const p = getUser(1);
p
.then(user => getRepositories(user.githubUsername))
.then(repos => getCommits(repos[0]))
.then(commits => console.log("Commits", commits))
.catch(err => console.log(err.message)); // best practice
console.log("After");

// Video 7

function getUser(id)
{
    return new Promise((resolve, reject) => {
        setTimeout(() => {
            console.log("Reading a user from database..");
            resolve({id: id, githubUsername: "moneebullah25"});
        }, 2000);
    });
}

// Exercise
function getRepositories(username)
{
    return new Promise((resolve, reject) => {
        setTimeout(() => {
            console.log("Getting user repository");
            resolve(['repo 1', 'repo 2', 'repo 3']);
        }, 2000);
    });
}

function getCommits(repository)
{
    return new Promise((resolve, reject) => {
        setTimeout(() => {
            console.log("Getting Commits"); 
            resolve(['commit 1', 'comit 2']);
        }, 2000);
    });
}