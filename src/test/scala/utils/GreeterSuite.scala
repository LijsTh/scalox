package utils 

class GreeterSuite extends munit.FunSuite {

  // Basic greeting test 
  test ("greet returns correct greeting") {
    val greeter = Greeter("Alice")
    assertEquals(greeter.greet(), "Hello, Alice!")
  }

}