import scala.io.Source
import scala.io.StdIn

import scala.collection.mutable.MutableList

import scala.annotation.switch
import scala.annotation.tailrec

object Brainfuck {

    // Called on launch. Runs a programme from a file, if one is passed as an argument. Otherwise, runs Hello World
    def main(args: Array[String]) {
        if(args.length < 1) {
            println("Invalid arguments: No input file specified. Running demo code...")
            execute("++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
        }    
        else runFile(args(0))
    }

    // Helper method that reads a script file into the interpreter
    def runFile(path : String) {
        val data = Source.fromFile(path).mkString
        execute(data)
    }

    // This method includes the actual interpreter
    def execute(code: String) {
        val inst = code.toCharArray
        val mem = MutableList.fill(1048576)(0x00)
        var pointer = 0
        nextStep(0)

        // Loops through the instruction array
        @tailrec def nextStep(i : Int) {
            if(i < inst.length) {
                val state = execInstruction(inst(i))
                if(state == 1)
                    nextStep(jumpForward(i))
                else if(state == (-1))
                    nextStep(jumpBackward(i))
                else
                    nextStep(i + 1)
            }
        }

        // Executes the instruction represented by the given character and returns 0 for a forward step, 1 for a forward jump, and -1 for a backward jump
        def execInstruction(instruction : Char) = {
            (instruction: @switch) match {
                case '>' => {
                    pointer += 1
                    if(pointer >= mem.length-2)
                        mem += 0x00
                    0
                }
                case '<' => {
                    if(pointer >= 0)
                        pointer -= 1
                    0
                }
                case '+' => {
                    if(mem(pointer) < 0xFF)
                        mem(pointer) += 1
                    0
                }
                case '-' => {
                    if(mem(pointer) > 0x00)
                        mem(pointer) -= 1
                    0
                }
                case '.' => {
                    print(mem(pointer).toChar)
                    0
                }
                case ',' => {
                    mem(pointer) = StdIn.readChar.toByte
                    0
                }
                case '[' => 1
                case ']' => (-1)
                case _ => 0
            }
        }

        // Called when the interpreter hits a forward jump instruction "[". Returns the new address of the instruction pointer.
        def jumpForward(i : Int) : Int = {
            if(mem(pointer) == 0x00) {
                @tailrec def check(j : Int, skip : Int) : Int =
                if(inst(j) == '[')
                    check(j + 1, skip + 1)
                else if (inst(j) == ']' && skip == 0)
                    j + 1
                else if (inst(j) == ']' && skip > 0)
                    check(j + 1, skip - 1)
                else
                    check(j + 1, skip)
                check(i + 1, 0)
            }
            else i + 1
        }

        // Called when the interpreter hits a backward jump instruction "]". Returns the new address of the instruction pointer.
        def jumpBackward(i : Int) : Int = {
            if(mem(pointer) != 0x00) {
                @tailrec def check(j : Int, skip : Int) : Int =
                if(inst(j) == ']')
                    check(j - 1, skip + 1)
                else if (inst(j) == '[' && skip == 0)
                    j + 1
                else if (inst(j) == '[' && skip > 0)
                    check(j - 1, skip - 1)
                else 
                    check(j - 1, skip)
                check(i - 1, 0)
            }
            else i + 1
        }
    }        
}//