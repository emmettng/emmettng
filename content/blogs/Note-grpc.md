---
title: "Note: grpc"
date: 2019-12-18T15:25:39+08:00
draft: true
---

> This summary follows the minimum useable principle.

#### Path 
- [grpc python doc](https://grpc.io/docs/tutorials/basic/python/)
- [grpc helloworld](https://github.com/grpc/grpc/tree/v1.25.0/examples/python/helloworld)
- [protocol buffer overview](https://developers.google.com/protocol-buffers/docs/overview)
  
## Simple example one

- Create file **`example.proto`** in folder `hpi/protos`.
  > generate python files
  > ```
  > > cd hpi 
  > > python -m grpc_tools.protoc -I./protos --python_out=. --grpc_python_out=. ./protos/hpi.proto
  > ```
  > get two files: `example_pb2.py` and `example_pb2_grpc.py` 

 
**A simple `.proto` file contains 3 parts**

1. This part almost never change, leave it alone
```
syntax = "proto3";
option java_multiple_files = true;
option java_package = "io.grpc.examples.hpi";
option java_outer_classname = "HPIProto";
option objc_class_prefix = "HLW";
```

2. package name :
```
package Example;        // package name. generate python fille 
```

3. Define `data type` and `function`
```
// Simple Example One

service ServiceOne {        
    rpc SimpleFunctionOne(SimpleTypeOne) returns (SimpleTypeTwo){}
}

message SimpleTypeOne{
    float floatRecorder = 1;
    int32 intRecorder = 2;
}

message SimpleTypeTwo{
    string stringRecorder = 1;
    repeated float floatListRecorder = 2;
}
```
- In `message SimpleTypeOne` , this `SimpleTypeOne` is just like `Type Constructor` in haskell. 
- `float floatRecorder =1 ` is like the value constructor.
  
```
data SimpleTypeOne = SimpleTypeOne{
    floatRecorder :: Float,
    intRecorder :: Int
}
```
- Function `SimpleFunctionOne` defined by input and return datatype. Concrete implementation will be specified in following section.
 
**In `example_pb2_grpc.py`** we can see:

1. it depends on the `grpc` package and the auto-generated `example_pb2`
```
import grpc
import hpi.example_pb2 as example__pb2
```

1. a class `ServiceOne`Servicer and a function `SimpleFunctionOne` as its member.
```
class ServiceOneServicer(object):
  """
  """

  def SimpleFunctionOne(self, request, context):
    # missing associated documentation comment in .proto file
    pass
    context.set_code(grpc.StatusCode.UNIMPLEMENTED)
    context.set_details('Method not implemented!')
    raise NotImplementedError('Method not implemented!')
```

2. A function add_`ServiceOne`Servicer_to_server. This function is the link between our actually implementation of `SimplefunctionOne` and its type definition and serilization in `example_pb2`.
```
def add_ServiceOneServicer_to_server(servicer, server):
  rpc_method_handlers = {
      'SimpleFunctionOne': grpc.unary_unary_rpc_method_handler(
          servicer.SimpleFunctionOne,
          request_deserializer=example__pb2.SimpleTypeOne.FromString,
          response_serializer=example__pb2.SimpleTypeTwo.SerializeToString,
      ),
  }
  generic_handler = grpc.method_handlers_generic_handler(
      'Example.ServiceOne', rpc_method_handlers)
  server.add_generic_rpc_handlers((generic_handler,))
```

**We write `example_server.py` to define details of `SimpleFunctionOne`**
```
from concurrent import futures
import time
import logging
import grpc
import hpi.example_pb2 as pb
import hpi.example_pb2_grpc as pb_grpc

_ONE_DAY_IN_SECONDS = 60 * 60 * 24

class LocalImplementation(pb_grpc.ServiceOneServicer):

    def SimpleFunctionOne(self, request, context):

        ## parse input from type SimpleTypeOne
        inputFloat = request.floatRecorder
        inputInt = request.intRecorder

        ## Business logic
        msg = "This input float is %s" % (str(inputFloat),)
        outputList = list(map(lambda x: float(x),[inputInt]*10))

        ## construct return value of type SimpleTypeTwo
        r = pb.SimpleTypeTwo(stringRecorder =msg, floatListRecorder=outputList)

        ## Return the value of business logic
        return r

def serve():
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    pb_grpc.add_ServiceOneServicer_to_server(LocalImplementation(), server)
    server.add_insecure_port('[::]:50051')
    server.start()
    try:
        while True:
            time.sleep(_ONE_DAY_IN_SECONDS)
    except KeyboardInterrupt:
        server.stop(0)

if __name__ == '__main__':
    logging.basicConfig()
    serve()
```

0. Rename `example_pb2.py` and `example_pb2_grpc.py` as `pd` and `pd_grpc`.
1. `pb_grpc.ServiceOneService` is the super class of `LocalImplementation`.
2. Overload `simpleFunctionOne` in `LocalImplementation`.
   1. Get function input using request.`recorder` style.
   2. Write business logic based on input values. 
   3. Construct return value `pb.SimpleTypeTwo` using `recorder` style.
3. Us return data constructor `pb.SimpleTypeTwo` from `pb`.
4. Function name (`SimpleFunctionOne`),type name(`pb.SimpleTypeTwo`), recorder name(`floatRecorder`, `stringRecorder`...) and Server name(`pb_grpc.ServiceOneServicer`) must be consist with what being defined in corresponding files.
5. Use `pb_grpc.add_ServiceOneServicer_to_server` to include this local implementation of `SimpleFunctionOne`.
6. Run this script, function `SimpleFunctionOne` is exposed by grpc call.
   

**We write `grpcExampleTest.py` to call remote function.**
```
from __future__ import print_function
import logging
import grpc

import hpi.example_pb2 as pb
import hpi.example_pb2_grpc as pb_grpc

def run():
    with grpc.insecure_channel('localhost:50051') as channel:
        ## Get rpc stub of remove function
        stub = pb_grpc.ServiceOneStub(channel)
        
        ## construct input for remote function
        inputV = pb.SimpleTypeOne(floatRecorder=20.1314,intRecorder=666)

        ## call remote function
        r = stub.SimpleFunctionOne(inputV)

        ## parse response with recorder style
        response1 = r.stringRecorder
        response2 = r.floatListRecorder

        print (response1)
        print (response2)

if __name__ == '__main__':
    logging.basicConfig()
    run()
```
- use `pb_grpc.ServiceOneStub` to get access to function `SimpleFunctionOne`
 
## Simple example Two
- Add more services and datatype in `hpi/protos/example.proto`
```
// Simple Example Two

service ServiceTwo{
    rpc vector2array(Vector) returns (Array){}
    rpc statistics (Array) returns (Statistics){}
}

message Vector {
    repeated float vector = 1;
}

message Array {
    repeated Vector array = 2;
}

message Statistics{
    repeated float stdList = 1;
    repeated float avgList = 2;
}
```
- Run 
  `python -m grpc_tools.protoc -I./protos --python_out=. --grpc_python_out=. ./protos/example.proto`

  A new class (`ServiceTwoServicer`) and and a related method(`add_ServiceTwoServicer`) will be add in `example_pb2_grpc.py`
  ```
  class ServiceTwoServicer(object):
  """Simple Example Two
  """
    def vector2array(self, request, context):
        ...
    def statistics(self, request, context):
        ...

  def add_ServiceTwoServicer_to_server(servicer, server):
    ...
  ```


**We add new class `localImplementationStat` in `example_server.py` to define details of functions `vector2array` and `statistics`**

```
import numpy as np 

class LocalImplementation(pb_grpc.ServiceOneServicer):

    def SimpleFunctionOne(self, request, context):
        ...

class LocalImplementationStat(pb_grpc.ServiceTwoServicer):
    def vector2array(self, request, context):

        ## unwrap(unboxing) input from type Vector
        ## get primitive datatype
        vector = request.vector

        ## Business logic
        rArray = [list(map(lambda x:x+i,vector)) for i in range(0,3)]

        ## wrap(boxing) primitive datatype
        ## get Array as defined in proto
        boxedArray = pb.Array(array = list(map(lambda x: pb.Vector(vector=x),rArray)))
        return boxedArray

    def statistics(self, request, context):
        ## unwrap(unboxing) input from type Vector
        ## get primitive datatype [[ ]]
        array =[v.vector for v in request.array]

        ## Business logic
        stdList = np.std(array,1)
        avgList = np.mean(array,1)

        ## wrap(boxing) primitive datatype
        ## get Statistics as defined in proto
        boxedStat= pb.Statistics(stdList=stdList,avgList=avgList)
        return boxedStat

def serve():
    ... 
    pb_grpc.add_ServiceTwoServicer_to_server(LocalImplementationStat(), server)
    ...
```
- First `unboxing` Data type to primitive data type
- Do business logic
- Boxing primitive data type based to whatever defined in `.proto`
- Use `pb_grpc,add_ServiceTwoServicer_toserver` to expose functions `vector2array` and `statistics` to grpc service.
  


**Add codes to `grpcExampleTest.py` to call `vector2array` and `statistics`.**
```
    ## Example Service Two
    stubTwo = pb_grpc.ServiceTwoStub(channel)
    inputVector = pb.Vector(vector=[1,2,3,3,4])

    boxedArray = stubTwo.vector2array(inputVector)
    print (boxedArray)
    boxedStat = stubTwo.statistics(boxedArray)
    print (boxedStat)
```
- Use `pb_grpc.ServiceTwoStub` to get access to remote functions.
- Boxing primitive value to what we defined in `proto`, in this case. We wrap a primitive python list to a `Vector` type.
- Call vector2Array and statistics.
- These two functions could composed without unwrapping(unboxing) the datatype.