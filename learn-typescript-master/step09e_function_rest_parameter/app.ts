function buildName(firstName: string, ...restOfName: string[]) {//Named function with Rest parameters
	// return firstName + " " + restOfName.join(",");
	console.log(restOfName[0]);
	console.log(restOfName[1]);
	console.log(restOfName[2]);
	// restOfName[3] = "undefined1232";
	if (restOfName[3]) console.log(restOfName[3]); // undefined
}

var employeeName = buildName("Joseph", "Samuel", "Lucas", "MacKinzie");

// console.log(employeeName); // Joseph Samuel,Lucas,MacKinzie


//anonymous function type with Rest parameters
var buildNameFun: (fname: string, ...rest: string[])=>string =
function (firstName: string, ...restOfName: string[]) {
	return firstName + " " + restOfName.join(" ");
}

//Note: Rest, optional and default parameters can only
// be at the end of the parameter list
											
						