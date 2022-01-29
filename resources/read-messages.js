// Collect messages from the currently focused channel. Messages are returned in the order they appear on screen, and look like {:author {:realname :user :host} :timestamp :html}
// This is separate from the rest because it has to be re-injected every time we switch chats.

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

let toMessage = elem => {
  let message_el = elem.querySelector('div[jsaction^=mouseenter][jslog*=impression] div[jscontroller]');
  //let embed = elem.querySelector('div[jsaction^=mouseenter] div[soy-server-key] a')?.outerHTML;
  //let html = embed || message_el?.innerHTML || '--ERROR message content missing--';
  let image = elem.querySelector('a span img')?.src || '';
  let text = message_el?.innerHTML || '';
  let html = text + (text && image ? '\n' : '') + image;
  return {
   author: toUser(elem.querySelector('span[data-member-id]')),
   timestamp: elem.querySelector('span[data-absolute-timestamp]').dataset.absoluteTimestamp.split('.')[0],
   html: html
  };
};

return map(toMessage, document.querySelectorAll('c-wiz[data-is-user-topic=true]'));
