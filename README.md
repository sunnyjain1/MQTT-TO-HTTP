mqtt_to_http
=====

Vernemq Plugin to convert Mqtt request to Http request.


## Prerequisites
    
You should have Git, Erlang 17.x and Vernemq installed in your PC to use this Plugin.
    
For Erlang 17.x you can download it from [here](https://www.erlang.org/downloads).
    
For Vernemq you can install from source file. [Here](https://github.com/erlio/vernemq) is the link to the source.

For Installing Git you can look [here](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git).
    
    
## Installation
    
    To install the plugin.

1) Clone the Directory using command
            
    $ git clone https://github.com/sunnyjain1/mqtt_to_http.git
    
2) Go in the clone directory using command
    
    $ cd mqtt_to_http
    
3) Compile the plugin using command
    
    $ ./rebar3 compile

4) Start the server using command
    
    $ vernemq start
    
5) Enable the plugin using command
    
    $ vmq-admin plugin enable -n mqtt_to_http -p <Cloned Dir>/_build/default/lib/mqtt_to_http
    
6) This plugin implements the hooks which are used by vmq_acl plugin so to disable vmq_acl plugin use command
    
    $ vmq-admin plugin disable -n vmq_acl
    
7) You can check the plugin starts working by using the command
    
    $ vmq-admin plugin show

## Reference

Reference used are:
    
Vernemq [docs](https://verne.mq/docs/).
    
Demo Plugin [link](https://github.com/dergraf/vernemq_demo_plugin).



## License

For License read LICENSE file.
