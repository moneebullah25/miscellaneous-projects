// Video 3
console.log("Before");
getUser(1, getRepository);

function getRepository(user)
{
    console.log("User ", user);
    getRepositories(user.githubUsername, getCommit);
}

function getCommit(repo)
{
    console.log("Repositories ", repo);
    getCommits(repo[0], displayCommit);
}

function displayCommit(commits)
{
    console.log(commits);
}

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
        console.log("Getting User Repositories");
        callback(['repo 1', 'repo 2', 'repo 3']);
    }, 2000);
}

function getCommits(repository, callback)
{
    setTimeout(() => {
        console.log("Getting Commits"); 
        callback(['commit 1', 'comit 2']);
    }, 2000);
}
