import scala.io.Source
import scala.io.StdIn

import scala.collection.mutable.MutableList

import scala.annotation.switch
import scala.annotation.tailrec


object Brainfuck {
    def main(args: Array[String]) {
        if(args.length < 1) {
            println("Invalid arguments: No input file specified")
        }    
        else {
            try {
                runFile(args(0))
            } 
            catch {
                case _ : Throwable => println("Unknow runtime error")
            }
        }
    }

    def runFile(path : String) {
        val data = Source.fromFile(path).mkString
        execute(data)
    }

    def execute(code: String) {
        val inst = code.toCharArray
        val mem = MutableList.fill(1048576)(0x00)
        var memp = 0
        def execSimple(inst : Char) = {
            (inst: @switch) match {
                case '>' => {
                    memp += 1
                    if(memp >= mem.length-2)
                        mem += 0x00
                    0
                }
                case '<' => {
                    if(memp >= 0)
                        memp -= 1
                    0
                }
                case '+' => {
                    if(mem(memp) < 0xFF)
                        mem(memp) += 1
                    0
                }
                case '-' => {
                    if(mem(memp) > 0x00)
                        mem(memp) -= 1
                    0
                }
                case '.' => {
                    print(mem(memp).toChar)
                    0
                }
                case ',' => {
                    mem(memp) = StdIn.readChar.toByte
                    0
                }
                case '[' => 1
                case ']' => (-1)
                case _ => 0
            }
        }

        def jumpForward(i : Int) : Int = {
            if(mem(memp) == 0x00) {
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
            else
                i + 1
        }

        def jumpBackward(i : Int) : Int = {
            if(mem(memp) != 0x00) {
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
            else
                i + 1
        }

        @tailrec def loop(i : Int) {
            if(i < inst.length) {
                val state = execSimple(inst(i))
                if(state == 1)
                    loop(jumpForward(i))
                else if(state == (-1))
                    loop(jumpBackward(i))
                else
                    loop(i + 1)
            }
        }
        loop(0)
    }
}