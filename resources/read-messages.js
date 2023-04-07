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

let toImageUrl = url => {
  return url.replaceAll('DOWNLOAD_URL', 'FIFE_URL').replaceAll(/(auto=true|sz=w[0-9]+)/g, 'sz=w1920');
}

let getTextForImage = div => {
  if (!div) {
    return '';
  }
  let img = div.querySelector('img');
  return '' +
    (div.title ? '[' + div.title + '] ' : '') +
    (img.src ? toImageUrl(img.src) : '--image url missing--');
}

// This is a gross hack.
// When a thread is created, the thread handle appears indented under the parent
// message.
// The thread timestamp is attached to the thread handle, and the channel timestamp
// is updated accordingly.
// However, we have no good way of selecting the thread handle, let alone pulling
// out the thread contents, yet.
// In the meantime, we simply say that the timestamp of a message is the greatest
// timestamp of the message itself or any of its child threads.
// This works around an issue where list-all-channels returns the channel timestamp
// based on the thread ts but read-messages-since doesn't return any messages with
// that ts so it gets stuck in a loop of thinking it has unread messages.
let getTimestamp = msg => {
  return map(
    x => x.dataset.absoluteTimestamp.split('.')[0],
    msg.querySelectorAll('span[data-absolute-timestamp]')).sort().pop();
}

let toMessage = elem => {
  let message_el = elem.querySelector('div[jsaction*=mouseenter][jslog*=impression] div[jscontroller]');
  //let embed = elem.querySelector('div[jsaction^=mouseenter] div[soy-server-key] a')?.outerHTML;
  //let html = embed || message_el?.innerHTML || '--ERROR message content missing--';
  let image = getTextForImage(elem.querySelector('[aria-label*=Image]'));
  let text = message_el?.innerHTML || '';
  let html = text + (text && image ? '\n' : '') + image;
  return {
   author: toUser(elem.querySelector('span[data-member-id]')),
   timestamp: getTimestamp(elem),
   html: html || '--ERROR: message content missing--'
  };
};

return map(toMessage, document.querySelectorAll('c-wiz[data-is-user-topic=true]'));
