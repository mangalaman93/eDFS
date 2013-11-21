eDFS
====

Erlang Distributed File System

[![Build Status](https://travis-ci.org/mangalaman93/eDFS.png?branch=feature_basic_fs)](https://travis-ci.org/mangalaman93/eDFS)

Dependencies
------------
* Erlang on unix machine
* Rebar
* make utility

How to Run
----------
This guide shows how to run eDFS master, worker, client and test examples
all at the same time on localhost. We begin with master-
```
$ pwd
/home/aman/Desktop/eDFS/master
$ make shell
(master@127.0.0.1)1> application:ensure_all_started(edfsm).
```

Run at least 3 workers by running following commands on 3 terminals-
```
$ pwd
/home/aman/Desktop/eDFS/worker
$ export PID=$$
$ export PORT=$PID
$ make shell
(worker16078@127.0.0.1)1> application:ensure_all_started(edfsw).
```

Run client as follows-
```
$ pwd
/home/aman/Desktop/eDFS/client
$ make shell
(client@127.0.0.1)1> application:ensure_all_started(edfsc).
```

Run test as follows-
```
$ pwd
/home/aman/Desktop/eDFS/test
$ make shell
(test@127.0.0.1)1> test:run("numbers.txt", 25000, ",").
```

see TODO file for more details.

Documentation
-------------
- [Website](http://www.cse.iitb.ac.in/~amanmangal/External/btp/)
