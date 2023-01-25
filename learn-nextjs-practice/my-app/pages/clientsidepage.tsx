let str = "Client Side page";

export default function helloClient() {
    console.log("On Client side");
    return (
        <div>Hello from Client side str: {str}</div>
    );
}