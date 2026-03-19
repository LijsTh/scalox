package utils 

case class Greeter(name: String) {
    def greet(): String = s"Hello, $name!"
}
