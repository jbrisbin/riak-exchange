#!/usr/bin/env python
import time
import amqplib.client_0_8 as amqp

conn = amqp.Connection()
ch = conn.channel()

start = time.time()

for i in range(10000):
  msg = amqp.Message(
    "Message #%s" % i, 
    content_type="text/plain", 
    application_headers={
      "test": "header"
    }
  )
  ch.basic_publish(msg, "riak", "msg.%s" % i)

end = time.time()
print "Elapsed time: %s" % (10000 / (end - start))