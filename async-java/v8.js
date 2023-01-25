/* Creating Settles Promises */
const p = Promise.resolve(1);
p.then(result => console.log("Result", result));

/* new Error also prints Call Stack on the console */
const q = Promise.reject(new Error("Error"));
q.catch(result => console.log("Error", result.message));

const p1 = new Promise((resolve) => {
    setTimeout(() => {
        console.log("Async Operation 1");
        resolve(1);
    }, 2000);
});

const p2 = new Promise((resolve, reject) => {
    setTimeout(() => {
        console.log("Async Operation 2");
        //resolve(2);
        reject(new Error("Something failed"));
    }, 2000);
});

/* We are not waiting for 1st sync operation to complete
Both sync operations are started at once but not at same time
 */
// Promise.all([p1, p2])
// /* Result will be an array. If 1 async failed all promise rejected */
// .then(result => console.log(result))
// .catch(err => console.log(err.message));

Promise.race([p1, p2])
/* Result will be an array. If 1 async passed it return immidiatly */
.then(result => console.log(result))
.catch(err => console.log(err.message));