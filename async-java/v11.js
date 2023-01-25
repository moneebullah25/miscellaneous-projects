/* await / async let to write asynchronous code synchronous way */

async function display(){
    /* To catch error we use try-catch block */
    try {
        const user = await getUser(1);
        const repos = getRepositories(user.githubUsername);
        const commits = await getCommits(repos[0]);
        console.log(commits);
    } catch (err) {
        console.log("Error", err.message);
    }
}

display();

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
            // resolve(['repo 1', 'repo 2', 'repo 3']);
            reject(new Error("Couldn't get repositories"));
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