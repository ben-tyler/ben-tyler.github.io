self.addEventListener('install', function (e) {
  e.waitUntil(
    caches.open('yawellnessapp').then(function (cache) {
      return cache.addAll([
        '/',
        '/manifest.json',
        '/src/js/main.js',
        '/assets/images/dwyl.png',
        '/assets/images/signal_wifi_off.svg',
        '/src/css/styles.css',
      ]);
    })
  );
});

self.addEventListener('fetch', function (event) {
  event.respondWith(
    caches.match(event.request).then(function (response) {
      return response || fetch(event.request);
    })
  );
});
