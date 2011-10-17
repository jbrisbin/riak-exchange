#!/usr/bin/env python
from datetime import datetime
import amqplib.client_0_8 as amqp

conn = amqp.Connection()
ch = conn.channel()

timestamp = datetime.now().strftime("%y%m%d%H%M%S%f")
msg = amqp.Message(
  "{\"type\": \"presence\", \"user\": \"jonbrisbin\", \"timestamp\": \"%s\"}" % (datetime.now().strftime("%d/%m/%y %H:%M")), 
  content_type="application/json", 
  application_headers={
    "Location": "Chicago",
    "X-Riak-Bucket": "auditlog",
    "X-Riak-Key": timestamp
  }
)
ch.basic_publish(msg, "auditlog", timestamp)
