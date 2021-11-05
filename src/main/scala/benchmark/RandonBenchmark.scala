package benchmark

import io.flow.util.{IdGenerator, OldIdGenerator}
import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.infra.Blackhole

class RandonBenchmark {

  @Benchmark
  def testNew(blackhole: Blackhole): String = {
    val s = IdGenerator("tst").randomId()
    blackhole.consume(s)
    s
  }

  @Benchmark
  def testOld(blackhole: Blackhole): String = {
    val s = OldIdGenerator("tst").randomId()
    blackhole.consume(s)
    s
  }

}

// Results:

// sbt:lib-util> jmh:run -i 10 -wi 10 -f1 -t1
//[warn] sbt 0.13 shell syntax is deprecated; use slash syntax instead: Jmh / run
//[info] running (fork) org.openjdk.jmh.Main -i 10 -wi 10 -f1 -t1
//[info] # JMH version: 1.32
//[info] # VM version: JDK 13.0.2, OpenJDK 64-Bit Server VM, 13.0.2+8
//[info] # VM invoker: /Library/Java/JavaVirtualMachines/adoptopenjdk-13.jdk/Contents/Home/bin/java
//[info] # VM options: <none>
//[info] # Blackhole mode: full + dont-inline hint
//[info] # Warmup: 10 iterations, 10 s each
//[info] # Measurement: 10 iterations, 10 s each
//[info] # Timeout: 10 min per iteration
//[info] # Threads: 1 thread, will synchronize iterations
//[info] # Benchmark mode: Throughput, ops/time
//[info] # Benchmark: benchmark.RandonBenchmark.testNew
//[info] # Run progress: 0.00% complete, ETA 00:06:40
//[info] # Fork: 1 of 1
//[info] # Warmup Iteration   1: 157796.023 ops/s
//[info] # Warmup Iteration   2: 165678.407 ops/s
//[info] # Warmup Iteration   3: 196551.453 ops/s
//[info] # Warmup Iteration   4: 508004.826 ops/s
//[info] # Warmup Iteration   5: 893807.583 ops/s
//[info] # Warmup Iteration   6: 1067920.253 ops/s
//[info] # Warmup Iteration   7: 1099165.025 ops/s
//[info] # Warmup Iteration   8: 1128792.620 ops/s
//[info] # Warmup Iteration   9: 1170719.694 ops/s
//[info] # Warmup Iteration  10: 1192360.745 ops/s
//[info] Iteration   1: 1176634.078 ops/s
//[info] Iteration   2: 1182904.138 ops/s
//[info] Iteration   3: 1186154.196 ops/s
//[info] Iteration   4: 1193607.578 ops/s
//[info] Iteration   5: 1179792.651 ops/s
//[info] Iteration   6: 1186488.158 ops/s
//[info] Iteration   7: 1171072.848 ops/s
//[info] Iteration   8: 1186245.844 ops/s
//[info] Iteration   9: 1187577.871 ops/s
//[info] Iteration  10: 1184289.295 ops/s
//[info] Result "benchmark.RandonBenchmark.testNew":
//[info]   1183476.666 ±(99.9%) 9543.184 ops/s [Average]
//[info]   (min, avg, max) = (1171072.848, 1183476.666, 1193607.578), stdev = 6312.225
//[info]   CI (99.9%): [1173933.482, 1193019.850] (assumes normal distribution)
//[info] # JMH version: 1.32
//[info] # VM version: JDK 13.0.2, OpenJDK 64-Bit Server VM, 13.0.2+8
//[info] # VM invoker: /Library/Java/JavaVirtualMachines/adoptopenjdk-13.jdk/Contents/Home/bin/java
//[info] # VM options: <none>
//[info] # Blackhole mode: full + dont-inline hint
//[info] # Warmup: 10 iterations, 10 s each
//[info] # Measurement: 10 iterations, 10 s each
//[info] # Timeout: 10 min per iteration
//[info] # Threads: 1 thread, will synchronize iterations
//[info] # Benchmark mode: Throughput, ops/time
//[info] # Benchmark: benchmark.RandonBenchmark.testOld
//[info] # Run progress: 50.00% complete, ETA 00:03:20
//[info] # Fork: 1 of 1
//[info] # Warmup Iteration   1: 542941.753 ops/s
//[info] # Warmup Iteration   2: 252221.218 ops/s
//[info] # Warmup Iteration   3: 99729.496 ops/s
//[info] # Warmup Iteration   4: 71666.228 ops/s
//[info] # Warmup Iteration   5: 69171.547 ops/s
//[info] # Warmup Iteration   6: 66756.324 ops/s
//[info] # Warmup Iteration   7: 66392.421 ops/s
//[info] # Warmup Iteration   8: 67804.005 ops/s
//[info] # Warmup Iteration   9: 66459.177 ops/s
//[info] # Warmup Iteration  10: 129579.297 ops/s
//[info] Iteration   1: 359324.515 ops/s
//[info] Iteration   2: 480985.996 ops/s
//[info] Iteration   3: 513358.863 ops/s
//[info] Iteration   4: 499886.008 ops/s
//[info] Iteration   5: 514781.258 ops/s
//[info] Iteration   6: 543899.691 ops/s
//[info] Iteration   7: 503316.344 ops/s
//[info] Iteration   8: 531210.308 ops/s
//[info] Iteration   9: 533086.101 ops/s
//[info] Iteration  10: 533744.223 ops/s
//[info] Result "benchmark.RandonBenchmark.testOld":
//[info]   501359.331 ±(99.9%) 80750.781 ops/s [Average]
//[info]   (min, avg, max) = (359324.515, 501359.331, 543899.691), stdev = 53411.642
//[info]   CI (99.9%): [420608.550, 582110.112] (assumes normal distribution)
//[info] # Run complete. Total time: 00:06:41
//[info] REMEMBER: The numbers below are just data. To gain reusable insights, you need to follow up on
//[info] why the numbers are the way they are. Use profilers (see -prof, -lprof), design factorial
//[info] experiments, perform baseline and negative tests that provide experimental control, make sur
//[info] the benchmarking environment is safe on JVM/OS/HW level, ask for reviews from the domain experts.
//[info] Do not assume the numbers tell you what you want them to tell.
//[info] Benchmark                 Mode  Cnt        Score       Error  Units
//[info] RandonBenchmark.testNew  thrpt   10  1183476.666 ±  9543.184  ops/s
//[info] RandonBenchmark.testOld  thrpt   10   501359.331 ± 80750.781  ops/s
//[success] Total time: 404 s (06:44), completed Nov 5, 2021, 5:37:46 PM