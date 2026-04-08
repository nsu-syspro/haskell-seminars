import Data.Function ((&))
--import Control.Applicative ((<**>))

(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (&)

-- Type checks, but does not apply effects in the right order
--(<**>) = flip (<*>)

when :: Applicative f => Bool -> f () -> f ()
when b x = if b then x else pure ()

unless :: Applicative f => Bool -> f () -> f ()
unless b x = if not b then x else pure ()
