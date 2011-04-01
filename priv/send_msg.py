#!/usr/bin/env python
from datetime import datetime
import amqplib.client_0_8 as amqp

conn = amqp.Connection()
ch = conn.channel()

msg = amqp.Message(
  "The time is now: %s" % (datetime.now().strftime("%d/%m/%y %H:%M")), 
  content_type="text/plain", 
  application_headers={
    "test": "header"
  }
)
ch.basic_publish(msg, "riak", "test")