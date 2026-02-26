#ifndef _service_h_
#define _service_h_

#define SERVER_SERVICE_PRIORITY 1

class Service {
 public:
  virtual void init() { reinit(); }
  virtual void reinit() {}
  virtual void start() {}
  virtual void stop() {}

  Service *next;
  int priority;  // higher priorities are run later

  Service(int apriority = 0);

  static Service *registered;
  static Vec<Service *> services;

  static void start_all();
  static void reinit_all();
  static void stop_all();
};

#endif
