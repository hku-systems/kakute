## Spark-DFT

### Building the project

1. Modify JDK source code (to avoid a bug of phosphor)

   ```bash
   vim /usr/lib/jvm/java-1.8.0-openjdk-amd64/src.zip -> UnixLoginModule.java
   ```

   in file UnixLoginModule.java

   delete the following code

   ```java
   // This code will cause bug in phosphor
   if (ss.getGroups() != null && ss.getGroups().length > 0) {
     unixGroups = ss.getGroups();
     for (int i = 0; i < unixGroups.length; i++) {
       UnixNumericGroupPrincipal ngp =
         new UnixNumericGroupPrincipal
         (unixGroups[i], false);
       if (!ngp.getName().equals(GIDPrincipal.getName()))
         supplementaryGroups.add(ngp);
     }
   }
   ```


2. build phosphor

   ```bash
   git clone https://github.com/jianyu-m/phosphor.git
   cd phosphor
   mvn verify
   ```

3. build spark

   ```bash
   git clone https://github.com/jianyu-m/spark.git
   ./build/mvn -DskipTests clean package
   ```





### Run the project

1. Set up the configuration file (dft.conf), a sample is

   ```ini
   # dft.conf
   host = 127.0.0.1
   port = 8787
   phosphor_java = /home/jianyu/phosphor/Phosphor/target/jre-inst-int
   phosphor_jar = /home/jianyu/phosphor/Phosphor-0.0.3-SNAPSHOT.jar
   phosphor_cache = dft-cache
   graph_dump_path = graph.dump
   ```

   change the phosphor_java and phosphor_jar dir to your coresponding phosphor dir

2. Start spark master and worker, then submit the task

3. stop the task and see the tracking dependency in graph.dump
