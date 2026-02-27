#include "common.h"

Service *Service::registered = 0;
Vec<Service *> Service::services;

static int compar_service(const void *a, const void *b) {
  Service *i = *(Service **)a;
  Service *j = *(Service **)b;
  if (i->priority > j->priority) return 1;
  if (i->priority < j->priority) return -1;
  return 0;
}

Service::Service(int apriority) {
  priority = apriority;
  next = registered;
  registered = this;
}

void Service::start_all() {
  for (Service *s = registered; s; s = s->next) services.add(s);
  qsort(&services.v[0], services.n, sizeof(services.v[0]), compar_service);
  for (Service *s : services) s->init();
  for (Service *s : services) s->start();
}

void Service::stop_all() { for (Service *s : services) s->stop(); }

void Service::reinit_all() { for (Service *s : services) s->reinit(); }
