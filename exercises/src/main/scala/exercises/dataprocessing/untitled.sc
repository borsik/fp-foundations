val str = "aaaaabaaabaaaab b"

def find(str: String): Int  = {
  var localMax = 0
  var global = 0
  var prev: Char = 'z'
  str.foreach { ch =>
    if (prev == 'z' || ch == prev) {
      localMax += 1
    } else {
      if (localMax > global)  {
        global = localMax
        localMax = 0
      }
    }
    prev = ch
  }
  global
}

find(str)