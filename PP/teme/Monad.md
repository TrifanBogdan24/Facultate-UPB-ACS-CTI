# `Monad` Design Pattern

## Monad in `C#`
> Link: https://www.youtube.com/shorts/C2-ljnsckrs

A Monad, in its simplest form, is a `wrapper` of an object of some type t.

In the list, the `Monad` is a `Design Pattern` that defines two functions:
- one to `create` a **Monad**
  being given the object
  and puts the object into it
- to `bind` a function to the **wrapped** object


The point of using the **Monad** is to put that boiler plate code into it,
so the **Monad** is doing other things that you would repetitively write all over the code.

A **Monad** is doing repeatitive doing
That element becomes the substance of the **Monad** you're writing.


```C#
class Monad where T : class
{
    public static Monad<T> Create(T obj);

    public Monad<TResult> Bind<TResult>;
}
```



## What is a monad? (Design Pattern)
> Link: https://www.youtube.com/watch?v=VgA4wCaxp-Q&t=2s

In programming, you often have to convert one value into another
by applying multiple operations one after the other.

Let's visualise this as a pipeline, where we apply succesive
function calls where the return value of each being input of the next.


Take a look at this **JavaScript** function where we want to
make a database query about a user, then get the gender of their best friend.

```JS
let username = "Gustave"
let userObject = database.fetch(username)
let userFriends = userObject.friends
let firstFriend = userFriends.first()
let firstFriendGender = userFriends.gender
```


In this case, any of these functions could fail,
such as if the database is own, or if the user has no friends.

Assuming that a failure results in `no value`,
it could be problematic passing a `null` into a function
that expects something else.


To remedy, you would probably add a check after each function call.

```JS
// imperative programming paradigm
let username = "Gustave"
let userObject = database.fetch(username)
if (userObject != null) {
    let userFriends = userObject.friends
    if (userFriends != null) {
        let firstFriend = userFriends.first()
        if (firstFriend != null) {
            let firstFriendGender = userFriends.gender
        }
    }
}
```


But this is **verbose** and frakly, quite ugly.
It also makes this code very hard to read
since you focus more on the implementation datails (`if` checks)
than the actual operation (`function calls`).



To refactor, let's create a class called `Maybe`.
Its `constructor` will simply take a value and store it inside the object instance.

We'll also give this class a method called `bind`,
which **takes a `function` as a `paramater`**.

This method, `bind`, will simply apply a function to its inner value and return a new instance of `Maybe`, with the new value.



```JS
class Maybe {
    constructor (value) {
        this.vlaue = value
    }

    bind = function(func) {
        value = func(this.value)
        return Maybe(value)
    }
}
```



Once we have this, we can rewrite our original pipeline by using
a set of **chained `bind` calls** of a `Maybe` insance.

```JS
let firstFriendGender = Maybe("Gustave")
    .bind(database.fetch)
    .bind(user => user.friends)
    .bind(friends => friends.first())
    .bind(friend => friend.gender)
```


You would notice that now there is
only place when functions are being called
which is within the `bind` **method**.

This means you can add extra logic and computation there
and it will apply at **every step** of the pipeline.


For this example, let's add a check to return itself
if the object contains a `null`.


```JS
class Maybe {
    constructor (value) {
        this.vlaue = value
    }

    bind = function(func) {
        if (this.value == null) {
            return this
        }
        value = func(this.value)
        return Maybe(value)
    }
}
```

Not only that this is a lot more readable,
but any changes can be made to just one place.

What we have is a `Design Pattern`, in which we abstract
pipeline implementation by wrapping a value in a type.


> Monad
>
> **Design Pattern** in which pipeline implementation are
> **abstracted** by **wrapping** a value in a **type**.


When you hear `Monad` in the context of computer science,
just think of it as being that.

So you might be wondering then why are these such a big deadl in `Haskell`?


Firstly, this code here:
```JS
// declarative code
let firstFriendGender = Maybe("Gustave")
    .bind(database.fetch)
    .bind(user => user.friends)
    .bind(friends => friends.first())
    .bind(friend => friend.gender)
```
describes what we want, not how to do it (**declarative**).

This makes the code **declarative**,
as opposed to the original **imperative** version.



As a **functional** language, `Haskell` loves its **declarative** code.

As such, it encourages the use of `monads` by providing built-ins
such as the `Maybe` `monad` **we create earlier**,
meaning you can use them without putting in the work of actually making them.

`Haskell` also has a special `bind` operator (`>>`)
which removes most of the clutter or dot bind calls.

```hs
let firstFriendGender = Maybe("Gustave")
    >>= database_fetch
    >>= \user -> get_user_friends(user)
    >>= \friends -> first(friends)
    >> \friend -> get_user_gender(friend)

-- this is probably invalid
```


In other languages, you can generally reassign variables and modify states,
but `Haskell` does not let you do this.

Thankfully, since **monads** are `Object` instances, they can store some extra data.

**Object**
- value
- history
- other_data


Let's modify our `JavaScript` example to keep a **log**
of every step of the pipelene.

```JS
class Monad {
    constructor (value, log=null) {
        this.value = value
        this.log = log || []    // log of pipeline steps
    }

    apply = function (func) {
        value = func(this.value)
        return Mand(value, log=[...this.log, value])
    }
}
```


The properties of this **monad** object effectively simulate
a sort of mutable state, still using **immutable** data.


```JS
let firstFriendGener = Mona("Gustave")
    .bind(database.fetch)
    // log = [dbObj]

    .bind(user => user.friends)
    // log = [dbObj, fList]

    .bind(friends => friends.first())
    // log [dbObj, fList, friendObj]

    .bind(friend => friend.gender)
```

