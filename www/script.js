// make all links open in new tab
$(document).on('click', 'a[href^=\"http\"]', function (e) {
  if (!this.hasAttribute('target')) {
    this.setAttribute('target', '_blank');
    this.setAttribute('rel', 'noopener noreferrer');
  }
});