let map = (f,xs) => Array.prototype.map.call(xs, f);

let toChat = span => {
  return {
    id: span.dataset.groupId,
    timestamp: span.querySelector('span[data-absolute-timestamp]')?.dataset.absoluteTimestamp.split('.')[0] || '0',
    topic: span.querySelector('span[role=presentation][title]').title,
    unread: span.innerText.includes('Unread'),
    users: [],
    count: 0,
    type: 'channel'
  };
};

return map(toChat, document.querySelectorAll('span[role=listitem]'));
