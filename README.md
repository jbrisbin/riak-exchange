# RabbitMQ Riak Exchange

Latest tagged version works with RabbitMQ 3.0.0 and Riak 1.2.1 (Riak Erlang client ver 1.3.1).

This is a custom exchange type for RabbitMQ that will put any message sent to it into Riak. 
By default, the Riak exchange will use your exchange name as the bucket name and your routing key as the Riak 
key. To alter this behaviour, simply pass special message headers:

* `X-Riak-Bucket` - set this to the bucket name to use.
* `X-Riak-Key` - set this to the key to use.

For example, setting AMQP messages headers like the following:

    {
      "X-Riak-Bucket": "riak-test",
      "X-Riak-Key": "mykey"
    }

Would result in a Riak object being stored at `/riak-test/mykey`

## Installation

To install from source:

    git clone https://github.com/jbrisbin/riak-exchange
    cd riak-exchange
    make deps
    make
    make package
    cp deps/*.ez $RABBITMQ_HOME/plugins
    cp dist/*.ez $RABBITMQ_HOME/plugins

Starting with RabbitMQ version 2.7, you must also enable the plugin using the new `rabbitmq-plugins` script.

Issuing...

    rabbitmq-plugins list

...should give you a list of the plugins available to enable:

    [ ] protobuffs                0.7.0
    [ ] rabbit_exchange_type_riak 0.1.9
    [ ] riak_pb                   1.2.0	
    [ ] riakc                     1.3.1

Enable the plugin (including dependencies) by executing:

    rabbitmq-plugins enable rabbit_exchange_type_riak
  
If you run `list` again, you should see them enabled:

   [e] protobuffs                        0.7.0
   [E] rabbit_exchange_type_riak         0.1.9
   [e] riak_pb                           1.2.0
   [e] riakc                             1.3.1

*NOTE:* I've also put up a tar file of the required .ez files you need to install in your RabbitMQ's plugins directory.

## Configuration

To use the Riak exchange type, declare your exchange as type "x-riak". In addition to forwarding messages to 
Riak, this also acts like a regular exchange so you can have consumers bound to this exchange and they will 
receive the messages as well as going to Riak.

To configure what Riak server to connect to, pass some arguments to the exchange declaration:

* `host` - Hostname or IP of the Riak server to connect to.
* `port` - Port number of the Riak server to connect to.
* `maxclients` - The maximum number of clients to create in the pool (use more clients for higher-traffic exchanges).

NEW in version 0.1.5: The Riak exchange can act like any valid RabbitMQ exchange type. Set an argument on your 
exchange when you declare it named `type_module` and give it a valid RabbitMQ exchange type module. You can use 
the ones built in that come with RabbitMQ, or you can use any custom ones you or a third party have written.

Here's a sample list of possible exchange types and what you should set the `type_module` value to:

* `direct`: `rabbit_exchange_type_direct`
* `fanout`: `rabbit_exchange_type_fanout`
* `headers`: `rabbit_exchange_type_headers`
* `topic`: `rabbit_exchange_type_topic`
* `random`: [`rabbit_exchange_type_random`](https://github.com/jbrisbin/random-exchange)

If you don't specify anything, the exchange will default to a topic exchange.

## Metadata

#### Content-Type

Whatever Content-Type you set on the message will be the Content-Type used for Riak.

#### Headers

The Riak exchange will also translate your custom AMQP message headers into the special `X-Riak-Meta-` 
headers required by Riak. For example, if you set a custom header with the name `customheader`, your Riak 
document will have a header named `X-Riak-Meta-customheader`.
