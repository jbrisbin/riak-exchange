# RabbitMQ Riak Exchange

This is a custom exchange type for RabbitMQ that will put any message sent to it into Riak. 
The name of your exchange becomes the bucket and the routing key you use will become the object key.

## Installation

To install from source:

    git clone https://github.com/jbrisbin/riak-exchange
    cd riak-exchange
    make deps
    make
    make package
    cp deps/*.ez $RABBITMQ_HOME/plugins
    cp dist/*.ez $RABBITMQ_HOME/plugins

I've also put up a tar file of the required .ez files you need to install in your RabbitMQ's plugins directory.

## Configuration

To use the Riak exchange type, declare your exchange as type "x-riak". In addition to forwarding messages to 
Riak, this is also a topic exchange so you can have consumers bound to this exchange and they will receive 
the messages as well as going to Riak.

To configure what Riak server to connect to, pass some arguments to the exchange declaration:

* `host` - Hostname or IP of the Riak server to connect to.
* `port` - Port number of the Riak server to connect to.
* `maxclients` - The maximum number of clients to create in the pool (use more clients for higher-traffic exchanges).

## Metadata

#### Content-Type

Whatever Content-Type you set on the message will be the Content-Type used for Riak.

#### Headers

The Riak exchange will translate your custom AMQP message headers into the special `X-Riak-Meta-` headers required 
by Riak. For example, if you set a custom header with the name `customheader`, your Riak document will have a 
header named `X-Riak-Meta-customheader`.