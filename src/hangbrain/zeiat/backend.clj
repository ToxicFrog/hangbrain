(ns hangbrain.zeiat.backend
  "Schema and protocol definitions for Zeiat backends."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    ))

(defschema BackendState s/Any)

(defschema Chat s/Any)

; LIST
; - get: nothing
; - need: irc name
; - want: real name, topic, user count
; NAMES
; - get: irc channel name
; - need: list of nicks on channel
; WHO
; - get: nothing
; - need: nick, user, host, realname
; JOIN
; - get: irc channel name
; - need: topic (can use real name instead), list of names

(defschema ChannelName
  "An IRC-compatible channel name. Only #channels are supported."
  (s/constrained
    s/Str
    #(re-matches #"#[^ ,]+" %)))

(defschema UserName
  "An IRC-compatible user name."
  ; Nominally this is any letter or one of [\]`_^{|}, followed by a run of those + digits and/or hyphens
  ; I take this a bit further and just say: it's not allowed to contain space, !, @, or :
  (s/constrained
    s/Str
    #(re-matches #"[^ !@:]+")))

(defschema Channel
  "Channel metadata returned by listChannels, listUnread, and statChannel."
  {:name ChannelName  ; IRC-compatible name of channel
   ; List of users on channel. May be empty on listChannels, but should be filled
   ; in if possible by statChannel.
   :users [UserName]
   ; Count is displayed in LIST and is nominally the number of users in the channel,
   ; but in some backends that is either very expensive to determine or not meaningful
   ; at all; in that case the backend may choose to send an "activity count" such as
   ; number of unread messages, or just send 0.
   :count s/Int
   ; Channel topic. Mandatory for statChannel. May be blank for listChannels, but any
   ; useful identifying information the backend can include here is welcome, e.g.
   ; the channel's human-readable name.
   (s/optional-key :topic) s/Str
   })

(defschema User
  "User metadata returned by listUsers and listUnread."
  {:name UserName
   :user UserName
   :host UserName
   :realname s/Str
   })

(defschema Chat
  ; TODO use cond-pre here, etc
  (s/either User Channel))

(defschema Message
  {:timestamp s/Int
   :author User
   :from Chat
   :to Chat
   :text s/Str})

(defprotocol ZeiatBackend
  "A protocol that Zeiat uses to communicate with whatever backend you connect to it. Library users should supply something that implements this protocol to zeiat/run."
  (connect [this] ;- nil
    "Connect to the backend. Called when a new user connects to Zeiat.")
  (disconnect [this] ;- nil
    "Disconnect from the backend. Called when a user quits or is otherwise disconnected.")
  (listChannels [this] ;- [Channel]
    "List all available channels. Called in response to a user's LIST command.")
  (listUsers [this]
    "List all available (DMable) users. Called in response to a user's WHO command.")
  (listUnread [this]
    "List all chats (users or channels) with unread messages. Called periodically to monitor for message traffic. Implementations can implement this however they want but note that just blindly returning all chats will result in a lot of unnecessary read-messages calls.")
  (statChannel [this channel]
    "Return information about a channel's users and read status.")
  (listMembers [this channel]
    "List all users in a given channel. See FIXME for the data shape. Called in response to NAMES or JOIN.")
  (readMessages [this channel]
    "Return all messages from the given channel available in the backscroll. Implementors can limit this to only what's easily available if convenient (e.g. return only history that was autoloaded, not all history). Calling this should mark the chat as read.")
  (readNewMessages [this channel]
    "As readMessages but should return only messages have not yet been read. Calling this should mark the chat as read.")
  (writeMessage [this channel message]
    "Send a message to the given channel or user. Calling this should mark the chat as read."))
