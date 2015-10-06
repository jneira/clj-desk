import java.util.concurrent.*;
import java.util.*;
import static java.util.Collections.*;

public class SimpleLocking {
    public static void main (String[] args) {
        long begin=System.currentTimeMillis();
        int nitems=100;
        int nthreads=10;
        final int niters=1000;
        final Node[] g=new Node[nitems];
        for(int i=0;i<nitems;i++) g[i]=new Node();
        ExecutorService pool=Executors.newFixedThreadPool(nthreads);
        Callable<Object>[] tasks=new Callable[nthreads];
        for (int i=0;i<nthreads;i++) {
            final int ii=i;
            tasks[i]=new Callable<Object>() {
                public Object call() {
                    int z=niters;
                    while (z-- > 0)
                        for(Node n : g) n.sum(ii+1);
                    return null;
                }};
        }
        try {
            Collection<Future<Object>> futures=
                pool.invokeAll(Arrays.asList(tasks));
            for (Future f : futures) f.get();
        }catch(Exception e){
            e.printStackTrace();
        }finally { pool.shutdown();}
        long sum=0; for (Node n : g) sum+=n.read();
        long end=System.currentTimeMillis();
        System.out.println("Sum node values: "+sum);
        System.out.println("Time: "+(end-begin));
    }

    public static class Node {
        //private Object m=new Object();
        private int n=0;
        public synchronized void sum(int x) {n=n+x;}
        public int read() {return n;}
    }
}



/**
int nitems=1000;
int nthreads=6;
final int niters=100000;
Sum node values: 2100000000
Time: 13812

int nitems=1000;
int nthreads=6;
final int niters=1000;
Sum node values: 21000000
Time: 125

user> (apply + (time (test-stm 1000 6 1000)))
"Elapsed time: 11207.386147 msecs"
21000000
89x mas lenta ouch!
*/
