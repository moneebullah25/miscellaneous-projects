// Video 2
console.log("v2");
const user = getUser(1);
console.log(user); // This will print undefined
console.log("v2 after");

/* To solve this problem we could user
    callbacks
    promises
    async/await syntactical sugar for promises
*/

function getUser(id)
{
    setTimeout(() => {
        console.log("Reading from Database...");
        return {id: id, githubUsername: "moneebullah25"};
    }, 2000);
}