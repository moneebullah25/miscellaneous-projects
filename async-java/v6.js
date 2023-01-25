/* Promise
    Object that hold the eventual result of an asynchronous operation
    when operation complete it return either resolve or reject 
    3 States Pending (when async operation is ongoing), 
             Fulfilled (when asyc operation is completed),
             Rejected (when asyc operation return error)
*/

const p = new Promise((resolve, reject) => {
    // Some async work setTimeout, network access or database
    setTimeout(() => {
        //resolve(1);
        reject(new Error("This is an Error"));
    }, 2000);
    //reject(1);
});

p
.then(result => console.log("Result",result))
.catch(err => console.log("Error", err.message))
.finally(() => console.log("Finally"));