package utils

class GreeterSuite extends munit.FunSuite:
  
  test("greet returns greeting with name"):
    val greeter = Greeter("Alice")
    assertEquals(greeter.greet, "Hello, Alice!")  
  