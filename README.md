# Lifeguard

**Lifeguard is still alpha quality and not ready for production use.**

Lifeguard is a highly scalable, fault tolerant distributed infrastructure and
application monitoring service. The health of your systems are determined by
checks written in JavaScript that run across the monitoring cluster. The data
that the checks inspect can be retrieved from various sources, such as a
database, Graphite, or remote SSH commands. Failing checks notify your IT
team as necessary.

## Features

Lifeguard brings monitoring to the modern age. Some features and a brief
description of each is given below:

* **Highly Scalable.** Monitoring systems such as Nagios very quickly run
  into scaling issues as the number of nodes and checks increases. With Lifeguard,
  you simply add more nodes and computation is evenly distributed across the
  entire cluster.

* **Fault Tolerant.** Lifeguard is master-less and can handle failure in many
  nodes.

* **Extensible.** Checks are all written in JavaScript and run using V8.
  Data sources and notifiers can be written in Erlang.
