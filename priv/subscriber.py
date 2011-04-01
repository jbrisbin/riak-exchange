#!/usr/bin/env python
import amqplib.client_0_8 as amqp

conn = amqp.Connection()
ch = conn.channel()

def callback(msg):
  print "msg: %s" % msg.body
  msg.channel.basic_ack(msg.delivery_tag)
  msg.channel.basic_cancel(msg.consumer_tag)

ch.access_request('/riak', active=True, read=True)

qname, _, _ = ch.queue_declare()
ch.queue_bind(qname, "riak", "test")
ch.basic_consume(qname, callback=callback)

while ch.callbacks:
  ch.wait()

ch.close()
conn.close()