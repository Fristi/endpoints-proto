Project itinere
---

Itinere comes from the latin word route. A while ago I already named a library itinere, which did routing as well. I didn't continue that as julienrf/endpoints library is superior to my library during that time.

This project is a extension of the algebra of Julien's library. 

I'll list below what features we have at the moment:

## Invariant functors on the algebra level

In the algebra of the endpoints library you'll see a lot of abstract types like: `Request[A]`, `RequestEntity[A]`, `Response[A]`, `QueryString[A]`, etcetera.

These are type constructors and describe some box holding a generic type. On the client side this box might look different as on the server side. However on the algebra level you could describe them as invariant.

Therefore we could require the interpreter to implement a instance of a `InvariantFunctor`. On the encoding side we use the contravariant function and on the decoding side we use the covariant function.

What's a use case for this? Well if you have a `final case class GameId(id: UUID) extends AnyVal` you want to map a `uuid` segment to a `GameId`. For that reason we'll need the `InvariantFunctor`. Also if you want you convert a `HList` to a case class you'll use `InvariantFunctor`. Both cases are using isomorphisms to convert to different representations.

Current issue: https://github.com/julienrf/endpoints/issues/56

## HList-based tupler instead of Scala tuples

I've used shapeless' hlist's to build up product types as seen here:
https://github.com/Fristi/endpoints-proto/blob/master/algebra/src/main/scala/itinere/Tupler.scala.

Advantages:
- No 22 tuple limit
- We can use the `Generic` machinery of shapeless to map to case class (see `Transformer.scala`)

## Improved http response

Project itinere has support for multiple responses per endpoint, which is a quite common use case. For example if some entity is not found we usually return a HTTP 404 NotFound or if the user supplies bad data, we return HTTP 403 BadRequest. 

For this we use a `coproductResponseBuilder` which allows you to build up a sum type of responses. At the algebra level we also need to a way to choose between responses. That's done with the `Cocartesian` instance on `HttpResponse`. A `Cocartesian` (coproducts) is the dual `Cartesian` (products). For products there exists a isomorphism between `HList` to case classes and tuples. For coproducts there exists a isomorphism between `Either` and a `Coproduct`. The `Coproduct` type should be aligned (alphabetically ordered) in order to work with the `Generic` shapeless machinery. For that reason we have a special case in https://github.com/Fristi/endpoints-proto/blob/master/algebra/src/main/scala/itinere/Transformer.scala#L11   

Also it's only possible to return the JSON entity, while you also might want to return response  headers. 

Current issue: https://github.com/julienrf/endpoints/issues/42


## JsonCodec adapter type class

When you want to support a new JSON library in Julien's endpoints library, you need to define support for each http server and client. This creates a high cardanality of modules. A simple solution is to create a adapter type class. With this solution you just need to define a module for each JSON library which converts the type class being used for encoding/decoding JSON to the uniform adapter interface.

A example can be found here: https://github.com/Fristi/endpoints-proto/blob/master/json-circe/src/main/scala/itinere/json/circe/CirceJsonCodec.scala#L21

It will use circe `Codec` to convert it into a itinere `JsonCodec`. We might want to split this up into `JsonEncoder` and `JsonDecoder` though.
 
## Http request headers

Julien's library only support basic authentication. In itinere we've build in request headers parsing: https://github.com/Fristi/endpoints-proto/blob/master/algebra/src/main/scala/itinere/Algebra.scala#L119

We could easily also do this for response headers. I'm also thinking to introduce a `StringCodec` which allows you to parse `Int`, `Double`, `UUID` and so on from a `String` as well you could encode and decode Base64, JSON, etc from that. This would make it easy to implement basic authenication.

## One algebra for docs and endpoints

Julien's library has a separate algebra for doing docs. I think this can be unified in one algebra, which has the descriptions as optional fields, which are left blank as default parameter.

## JsonSchema derivation through shapeless

I've created a simple ADT which describes json schema here: https://github.com/Fristi/endpoints-proto/blob/master/algebra/src/main/scala/itinere/JsonSchema.scala#L60

Julien has also done some work on this, but it's a bit different from what I've done. Both approaches could work and be split in libraries.

Current pull request: https://github.com/julienrf/endpoints/pull/53

## Http4s server

Julien's library doesn't have a implementation yet, but I think it's fairly easy to bring this over.

