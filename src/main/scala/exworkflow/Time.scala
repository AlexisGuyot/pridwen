object time {
    // Source : https://biercoff.com/easily-measuring-code-execution-time-in-scala/
    def time[R](block: => R): R = {
        val t0 = System.nanoTime()
        val result = block    // call-by-name
        val t1 = System.nanoTime()
        //println("Elapsed time: " + (t1 - t0) + " ns")
        result
    } 
}