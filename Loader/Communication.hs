-- |A communication layer for distributed processes wanting to exchange 
-- events about their lifecycle. This layer allows a Loader supervisor
-- to send and receive events from other processes, and dispatch control 
-- messages.
module Loader.Communication where
import Network.Socket

