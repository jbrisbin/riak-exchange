#!/usr/bin/env python
import amqplib.client_0_8 as amqp

conn = amqp.Connection()
ch = conn.channel()

msg = amqp.Message(
  "Hello World!", 
  content_type="text/plain", 
  application_headers={
    "test": "header"
  }
)
ch.basic_publish(msg, "riak", "test")