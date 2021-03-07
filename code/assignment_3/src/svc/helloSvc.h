/**
 * Autogenerated by Thrift Compiler (0.14.0)
 *
 * DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
 *  @generated
 */
#ifndef helloSvc_H
#define helloSvc_H

#include <thrift/TDispatchProcessor.h>
#include <thrift/async/TConcurrentClientSyncInfo.h>
#include <memory>
#include "hello_types.h"



#ifdef _MSC_VER
  #pragma warning( push )
  #pragma warning (disable : 4250 ) //inheriting methods via dominance 
#endif

class helloSvcIf {
 public:
  virtual ~helloSvcIf() {}
  virtual void getMessage(std::string& _return, const std::string& name) = 0;
};

class helloSvcIfFactory {
 public:
  typedef helloSvcIf Handler;

  virtual ~helloSvcIfFactory() {}

  virtual helloSvcIf* getHandler(const ::apache::thrift::TConnectionInfo& connInfo) = 0;
  virtual void releaseHandler(helloSvcIf* /* handler */) = 0;
};

class helloSvcIfSingletonFactory : virtual public helloSvcIfFactory {
 public:
  helloSvcIfSingletonFactory(const ::std::shared_ptr<helloSvcIf>& iface) : iface_(iface) {}
  virtual ~helloSvcIfSingletonFactory() {}

  virtual helloSvcIf* getHandler(const ::apache::thrift::TConnectionInfo&) {
    return iface_.get();
  }
  virtual void releaseHandler(helloSvcIf* /* handler */) {}

 protected:
  ::std::shared_ptr<helloSvcIf> iface_;
};

class helloSvcNull : virtual public helloSvcIf {
 public:
  virtual ~helloSvcNull() {}
  void getMessage(std::string& /* _return */, const std::string& /* name */) {
    return;
  }
};

typedef struct _helloSvc_getMessage_args__isset {
  _helloSvc_getMessage_args__isset() : name(false) {}
  bool name :1;
} _helloSvc_getMessage_args__isset;

class helloSvc_getMessage_args {
 public:

  helloSvc_getMessage_args(const helloSvc_getMessage_args&);
  helloSvc_getMessage_args& operator=(const helloSvc_getMessage_args&);
  helloSvc_getMessage_args() : name() {
  }

  virtual ~helloSvc_getMessage_args() noexcept;
  std::string name;

  _helloSvc_getMessage_args__isset __isset;

  void __set_name(const std::string& val);

  bool operator == (const helloSvc_getMessage_args & rhs) const
  {
    if (!(name == rhs.name))
      return false;
    return true;
  }
  bool operator != (const helloSvc_getMessage_args &rhs) const {
    return !(*this == rhs);
  }

  bool operator < (const helloSvc_getMessage_args & ) const;

  uint32_t read(::apache::thrift::protocol::TProtocol* iprot);
  uint32_t write(::apache::thrift::protocol::TProtocol* oprot) const;

};


class helloSvc_getMessage_pargs {
 public:


  virtual ~helloSvc_getMessage_pargs() noexcept;
  const std::string* name;

  uint32_t write(::apache::thrift::protocol::TProtocol* oprot) const;

};

typedef struct _helloSvc_getMessage_result__isset {
  _helloSvc_getMessage_result__isset() : success(false) {}
  bool success :1;
} _helloSvc_getMessage_result__isset;

class helloSvc_getMessage_result {
 public:

  helloSvc_getMessage_result(const helloSvc_getMessage_result&);
  helloSvc_getMessage_result& operator=(const helloSvc_getMessage_result&);
  helloSvc_getMessage_result() : success() {
  }

  virtual ~helloSvc_getMessage_result() noexcept;
  std::string success;

  _helloSvc_getMessage_result__isset __isset;

  void __set_success(const std::string& val);

  bool operator == (const helloSvc_getMessage_result & rhs) const
  {
    if (!(success == rhs.success))
      return false;
    return true;
  }
  bool operator != (const helloSvc_getMessage_result &rhs) const {
    return !(*this == rhs);
  }

  bool operator < (const helloSvc_getMessage_result & ) const;

  uint32_t read(::apache::thrift::protocol::TProtocol* iprot);
  uint32_t write(::apache::thrift::protocol::TProtocol* oprot) const;

};

typedef struct _helloSvc_getMessage_presult__isset {
  _helloSvc_getMessage_presult__isset() : success(false) {}
  bool success :1;
} _helloSvc_getMessage_presult__isset;

class helloSvc_getMessage_presult {
 public:


  virtual ~helloSvc_getMessage_presult() noexcept;
  std::string* success;

  _helloSvc_getMessage_presult__isset __isset;

  uint32_t read(::apache::thrift::protocol::TProtocol* iprot);

};

class helloSvcClient : virtual public helloSvcIf {
 public:
  helloSvcClient(std::shared_ptr< ::apache::thrift::protocol::TProtocol> prot) {
    setProtocol(prot);
  }
  helloSvcClient(std::shared_ptr< ::apache::thrift::protocol::TProtocol> iprot, std::shared_ptr< ::apache::thrift::protocol::TProtocol> oprot) {
    setProtocol(iprot,oprot);
  }
 private:
  void setProtocol(std::shared_ptr< ::apache::thrift::protocol::TProtocol> prot) {
  setProtocol(prot,prot);
  }
  void setProtocol(std::shared_ptr< ::apache::thrift::protocol::TProtocol> iprot, std::shared_ptr< ::apache::thrift::protocol::TProtocol> oprot) {
    piprot_=iprot;
    poprot_=oprot;
    iprot_ = iprot.get();
    oprot_ = oprot.get();
  }
 public:
  std::shared_ptr< ::apache::thrift::protocol::TProtocol> getInputProtocol() {
    return piprot_;
  }
  std::shared_ptr< ::apache::thrift::protocol::TProtocol> getOutputProtocol() {
    return poprot_;
  }
  void getMessage(std::string& _return, const std::string& name);
  void send_getMessage(const std::string& name);
  void recv_getMessage(std::string& _return);
 protected:
  std::shared_ptr< ::apache::thrift::protocol::TProtocol> piprot_;
  std::shared_ptr< ::apache::thrift::protocol::TProtocol> poprot_;
  ::apache::thrift::protocol::TProtocol* iprot_;
  ::apache::thrift::protocol::TProtocol* oprot_;
};

class helloSvcProcessor : public ::apache::thrift::TDispatchProcessor {
 protected:
  ::std::shared_ptr<helloSvcIf> iface_;
  virtual bool dispatchCall(::apache::thrift::protocol::TProtocol* iprot, ::apache::thrift::protocol::TProtocol* oprot, const std::string& fname, int32_t seqid, void* callContext);
 private:
  typedef  void (helloSvcProcessor::*ProcessFunction)(int32_t, ::apache::thrift::protocol::TProtocol*, ::apache::thrift::protocol::TProtocol*, void*);
  typedef std::map<std::string, ProcessFunction> ProcessMap;
  ProcessMap processMap_;
  void process_getMessage(int32_t seqid, ::apache::thrift::protocol::TProtocol* iprot, ::apache::thrift::protocol::TProtocol* oprot, void* callContext);
 public:
  helloSvcProcessor(::std::shared_ptr<helloSvcIf> iface) :
    iface_(iface) {
    processMap_["getMessage"] = &helloSvcProcessor::process_getMessage;
  }

  virtual ~helloSvcProcessor() {}
};

class helloSvcProcessorFactory : public ::apache::thrift::TProcessorFactory {
 public:
  helloSvcProcessorFactory(const ::std::shared_ptr< helloSvcIfFactory >& handlerFactory) :
      handlerFactory_(handlerFactory) {}

  ::std::shared_ptr< ::apache::thrift::TProcessor > getProcessor(const ::apache::thrift::TConnectionInfo& connInfo);

 protected:
  ::std::shared_ptr< helloSvcIfFactory > handlerFactory_;
};

class helloSvcMultiface : virtual public helloSvcIf {
 public:
  helloSvcMultiface(std::vector<std::shared_ptr<helloSvcIf> >& ifaces) : ifaces_(ifaces) {
  }
  virtual ~helloSvcMultiface() {}
 protected:
  std::vector<std::shared_ptr<helloSvcIf> > ifaces_;
  helloSvcMultiface() {}
  void add(::std::shared_ptr<helloSvcIf> iface) {
    ifaces_.push_back(iface);
  }
 public:
  void getMessage(std::string& _return, const std::string& name) {
    size_t sz = ifaces_.size();
    size_t i = 0;
    for (; i < (sz - 1); ++i) {
      ifaces_[i]->getMessage(_return, name);
    }
    ifaces_[i]->getMessage(_return, name);
    return;
  }

};

// The 'concurrent' client is a thread safe client that correctly handles
// out of order responses.  It is slower than the regular client, so should
// only be used when you need to share a connection among multiple threads
class helloSvcConcurrentClient : virtual public helloSvcIf {
 public:
  helloSvcConcurrentClient(std::shared_ptr< ::apache::thrift::protocol::TProtocol> prot, std::shared_ptr<::apache::thrift::async::TConcurrentClientSyncInfo> sync) : sync_(sync)
{
    setProtocol(prot);
  }
  helloSvcConcurrentClient(std::shared_ptr< ::apache::thrift::protocol::TProtocol> iprot, std::shared_ptr< ::apache::thrift::protocol::TProtocol> oprot, std::shared_ptr<::apache::thrift::async::TConcurrentClientSyncInfo> sync) : sync_(sync)
{
    setProtocol(iprot,oprot);
  }
 private:
  void setProtocol(std::shared_ptr< ::apache::thrift::protocol::TProtocol> prot) {
  setProtocol(prot,prot);
  }
  void setProtocol(std::shared_ptr< ::apache::thrift::protocol::TProtocol> iprot, std::shared_ptr< ::apache::thrift::protocol::TProtocol> oprot) {
    piprot_=iprot;
    poprot_=oprot;
    iprot_ = iprot.get();
    oprot_ = oprot.get();
  }
 public:
  std::shared_ptr< ::apache::thrift::protocol::TProtocol> getInputProtocol() {
    return piprot_;
  }
  std::shared_ptr< ::apache::thrift::protocol::TProtocol> getOutputProtocol() {
    return poprot_;
  }
  void getMessage(std::string& _return, const std::string& name);
  int32_t send_getMessage(const std::string& name);
  void recv_getMessage(std::string& _return, const int32_t seqid);
 protected:
  std::shared_ptr< ::apache::thrift::protocol::TProtocol> piprot_;
  std::shared_ptr< ::apache::thrift::protocol::TProtocol> poprot_;
  ::apache::thrift::protocol::TProtocol* iprot_;
  ::apache::thrift::protocol::TProtocol* oprot_;
  std::shared_ptr<::apache::thrift::async::TConcurrentClientSyncInfo> sync_;
};

#ifdef _MSC_VER
  #pragma warning( pop )
#endif



#endif
