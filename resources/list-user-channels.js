let map = (f,xs) => Array.prototype.map.call(xs, f);

let toUser = span => {
  let name = span.dataset.name;
  let [user,host] = span.dataset.hovercardId.split('@');
  return {
    realname: name,
    user: user,
    host: host,
  };
};

let toChat = span => {
  let id = span.dataset.groupId;
  let users = map(toUser, span.querySelectorAll('span[data-member-id]'));
  let timestamp = span.querySelector('span[data-absolute-timestamp]')?.dataset.absoluteTimestamp.split('.')[0] || '0';
  let unread = span.innerText.includes('Unread');
  if (users.length > 1) {
    let topic = span.querySelector('span[role=presentation][title]').title;
    return { id: id, users: users, topic: topic, timestamp: timestamp, unread: unread, count: users.length, type: 'channel' };
  } else if (users.length == 1) {
    return Object.assign(users[0], { id: id, timestamp: timestamp, unread: unread, type: 'dm' });
  } else {
    return false;
  }
};

return map(toChat, document.querySelectorAll('span[role=listitem]')).filter(x=>x);
