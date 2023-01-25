/* Asynchronous JS
    Each js file either with async js will use a single thread
    setTimeout function just schedule the task for later execution using a single thread

*/

// Video 1
console.log("1");
setTimeout(() => {
    console.log("Inside setTimeout for 2 sec")
}, 2000);
console.log("2");