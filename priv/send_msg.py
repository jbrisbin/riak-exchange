#!/usr/bin/env python
from datetime import datetime
import amqplib.client_0_8 as amqp

conn = amqp.Connection()
ch = conn.channel()

msg = amqp.Message(
  "{\"now\": \"%s\"}" % (datetime.now().strftime("%d/%m/%y %H:%M")), 
  content_type="application/json", 
  application_headers={
    "test": "header"
    # "X-Riak-Bucket": "test",
    # "X-Riak-Key": "test2"
  }
)
ch.basic_publish(msg, "rtest", "test2")