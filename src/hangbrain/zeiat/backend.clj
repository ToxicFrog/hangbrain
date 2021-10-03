(ns hangbrain.zeiat.backend
  "Schema and protocol definitions for Zeiat backends. If implementing a new backend, you should start here."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [clojure.string :as string]))

(defschema ChannelName
  "An IRC-compatible channel name. Only #channels are supported."
  (s/constrained
    s/Str
    #(re-matches #"#[^ ,]+" %)))

(defschema UserName
  "An IRC-compatible user name. This is somewhat more permissive than rfc2812, allowing any non-empty name that doesn't contain !@: or whitespace."
  (s/constrained
    s/Str
    #(re-matches #"[^ !@:]+" %)))

(defschema AnyName
  (s/conditional
    #(string/starts-with? % "#") ChannelName
    string? UserName))

(defschema User
  "User metadata returned by listUsers and listUnread."
  {:name UserName
   :user UserName
   :host UserName
   :realname s/Str
   ; Backend can store whatever additional information here is convenient
   s/Any s/Any
   })

(defschema Channel
  "Channel metadata returned by listChannels, listUnread, and statChannel."
  {:name ChannelName  ; IRC-compatible name of channel
   ; List of users on channel. May be empty on listChannels, but should be filled
   ; in if possible by statChannel.
   :users [User]
   ; Count is displayed in LIST and is nominally the number of users in the channel,
   ; but in some backends that is either very expensive to determine or not meaningful
   ; at all; in that case the backend may choose to send an "activity count" such as
   ; number of unread messages, or just send 0.
   :count s/Int
   ; Channel topic. Mandatory for statChannel. May be blank for listChannels, but any
   ; useful identifying information the backend can include here is welcome, e.g.
   ; the channel's human-readable name.
   (s/optional-key :topic) s/Str
   ; Backend can store whatever additional information here is convenient
   s/Any s/Any
   })

(defschema Chat
  "Something that can send and receive messages, i.e. either a User or a Channel."
  ; TODO use cond-pre here, etc
  (s/conditional
    :user User
    :users Channel))

(defschema ChatTarget
  "The target of a chat message; either a User or the special value :me to indicate the logged in user."
  (s/cond-pre (s/enum :me) User))

(defschema Message
  {; TODO should probably rename this "id" since it might not be a ts in all backends
   :timestamp s/Any
   ; Origin, should be :me if the author is the logged in user and the author otherwise
   :from (s/cond-pre (s/enum :me) User)
   ; Destination, should be :me for incoming DMs, the target user for outgoing DMs, and
   ; the target channel for channel messages
   :to (s/conditional
         keyword? (s/enum :me)
         :user User
         :users Channel)
   ; The actual text of the message
   :text s/Str
   ; Backend can store whatever additional information here is convenient, e.g.
   ; timestamps, the original HTML, whatever.
   s/Any s/Any
   })

(defprotocol ZeiatBackend
  "A protocol that Zeiat uses to communicate with whatever backend you connect to it. Library users should supply something that implements this protocol to zeiat/run."
  (connect [this] ;- nil
    ; TODO: this should take information about the registered user, so that the backend can
    ; make decisions based on that, and should return human-readable information for the
    ; frontend to display.
    "Connect to the backend. Called when a new user connects to Zeiat.")
  (disconnect [this] ;- nil
    "Disconnect from the backend. Called when a user quits or is otherwise disconnected.")
  (listChannels [this] ;- [Channel]
    "List all available channels. Called in response to a user's LIST command.")
  (listUsers [this] ;- [User]
    "List all available (DMable) users. Called in response to a user's WHO command.")
  (listUnread [this] ;- [Chat]
    "List all chats (users or channels) with unread messages. Called periodically to monitor for message traffic. Implementations can implement this however they want but note that just blindly returning all chats will result in a lot of unnecessary read-messages calls.")
  (statChannel [this channel] ;- Chat
    "Return information about a channel's users and read status.")
  (listMembers [this channel] ;- [User]
    "List all users in a given channel. See FIXME for the data shape. Called in response to NAMES or JOIN.")
  (readMessages [this channel] ;- [Message]
    "Return all messages from the given channel available in the backscroll. Implementors can limit this to only what's easily available if convenient (e.g. return only history that was autoloaded, not all history). Calling this should mark the chat as read.")
  (readNewMessages [this channel] ;- [Message]
    "As readMessages but should return only messages have not yet been read. Calling this should mark the chat as read.")
  (writeMessage [this channel message] ;- bool
    "Send a message to the given channel or user. Calling this should mark the chat as read. Returns true if the message was successfully sent, false otherwise."))

(defschema ^:private Backend (s/protocol ZeiatBackend))

;; Functions for checked calls to backend impls
(defn connect
  [this :- Backend]
  (.connect this))

(defn disconnect
  [this :- Backend]
  (.disconnect this))

(defn list-channels :- [Channel]
  [this :- Backend]
  (.listChannels this))

(defn list-users :- [User]
  [this :- Backend]
  (.listUsers this))

(defn list-members :- [User]
  [this :- Backend, channel :- ChannelName]
  (.listMembers this channel))

(defn list-unread :- [Chat]
  [this :- Backend]
  (.listUnread this))

(defn stat-channel :- Channel
  [this :- Backend, channel :- ChannelName]
  (.statChannel this channel))

(defn read-messages :- [Message]
  [this :- Backend, channel :- AnyName]
  (.readMessages this channel))

(defn read-new-messages :- [Message]
  [this :- Backend, channel :- AnyName]
  (.readNewMessages this channel))

(defn write-message :- s/Bool
  [this :- Backend, channel :- AnyName, msg :- s/Str]
  (.writeMessage this channel msg))
