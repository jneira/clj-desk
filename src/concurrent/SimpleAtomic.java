package concurrent;

import java.util.concurrent.*;

public class SimpleAtomic {
    long begin=System.currentTimeMillis();
    int nitems=1000;
    int nthreads=6;
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
