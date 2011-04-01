#!/usr/bin/env python
import amqplib.client_0_8 as amqp

conn = amqp.Connection()
ch = conn.channel()

for i in range(1000):
  msg = amqp.Message(
    "Message #%s" % i, 
    content_type="text/plain", 
    application_headers={
      "test": "header"
    }
  )
  ch.basic_publish(msg, "riak", "msg%s" % i)