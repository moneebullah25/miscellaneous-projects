function log(req, res, next) {
    console.log("Logging... ");
    next(); // if next not present, request will end up hanging
}; // Custom middleware
function authenticate(req, res, next) {
    console.log("Authenticating... ");
    next(); // if next not present, request will end up hanging
}; // Custom middleware

module.exports.logger = log;
module.exports.authenticate = authenticate;