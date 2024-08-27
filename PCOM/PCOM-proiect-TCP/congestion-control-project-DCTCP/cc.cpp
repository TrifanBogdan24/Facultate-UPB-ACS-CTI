// -*- c-basic-offset: 4; indent-tabs-mode: nil -*-
#include <math.h>
#include <iostream>
#include <algorithm>
#include "cc.h"
#include "queue.h"
#include <stdio.h>
#include "switch.h"
#include "ecn.h"
using namespace std;

////////////////////////////////////////////////////////////////
//  CC SOURCE. Aici este codul care ruleaza pe transmitatori. Tot ce avem nevoie pentru a implementa
//  un algoritm de congestion control se gaseste aici.
////////////////////////////////////////////////////////////////
int CCSrc::_global_node_count = 0;

CCSrc::CCSrc(EventList &eventlist)
    : EventSource(eventlist, "cc"), _flow(NULL)
{
    _mss = Packet::data_packet_size();
    _acks_received = 0;
    _nacks_received = 0;

    _highest_sent = 0;
    _next_decision = 0;
    _flow_started = false;
    _sink = 0;

    _node_num = _global_node_count++;
    _nodename = "CCsrc " + to_string(_node_num);

    _cwnd = 10 * _mss;
    _ssthresh = 0xFFFFFFFFFF;
    _flightsize = 0;
    _flow._name = _nodename;
    setName(_nodename);

    // DCTCP-specific initialization
    _alpha = 0;
    // valoarea recomandata in paperul dctcp
    _g = 1/16.0;
    _cwnd_floor =  4.0 * _mss;
    _cwnd_shrink_factor = 0.003;
    _cwnd_growth_factor = 2.0;
}

/* Starts the transmission of the byte flow */
void CCSrc::startflow() {
    cout << "Start flow " << _flow._name << " at " << timeAsSec(eventlist().now()) << "s" << endl;
    _flow_started = true;
    _highest_sent = 0;
    _packets_sent = 0;

    while (_flightsize + _mss < _cwnd)
        send_packet();
}

/* Initialize the connection to the sink host */
void CCSrc::connect(Route* routeout, Route* routeback, CCSink& sink, simtime_picosec starttime) {
    assert(routeout);
    _route = routeout;

    _sink = &sink;
    _flow._name = _name;
    _sink->connect(*this, routeback);

    eventlist().sourceIsPending(*this, starttime);
}

/* Variables we will work with:
    _nacks_received
    _flightsize -> number of bytes in flight
    _mss -> maximum segment size
    _next_decision
    _highest_sent
    _cwnd
    _ssthresh
    
    CCAck._ts -> ACK timestamp
    eventlist.now -> current time
    eventlist.now - CCAck._tx -> latency
    
    ack.ackno();
    
    > You can include any other variables in the rest of the code as needed.
*/

// This function is called when the queue size is exceeded and the packet with the sequence number ackno is dropped.
void CCSrc::processNack(const CCNack& nack) {
    // cout << "CC " << _name << " got NACK " <<  nack.ackno() << _highest_sent << " at " << timeAsMs(eventlist().now()) << " us" << endl;
    _nacks_received++;
    _flightsize -= _mss;

    if (nack.ackno() >= _next_decision) {
        // when receiving a packet loss
        _cwnd = max(_cwnd * _cwnd_shrink_factor, _cwnd_floor);
        _ssthresh = _cwnd;
        _next_decision = _highest_sent + _cwnd;
    }
}

/* Process an ACK. Mostly just housekeeping */
void CCSrc::processAck(const CCAck& ack) {
    CCAck::seq_t ackno = ack.ackno();

    _acks_received++;
    _flightsize -= _mss;

    _packets_count++;

    if (ack.is_ecn_marked()) {
        _ecn_packets_count++; // Increment ECN-marked packet count
    }

    if (ackno >= _next_decision) {

        // aici e formula din paperul care descrie dctcp
        // am pastrat fix ca in paper
        double F = static_cast<double>(_ecn_packets_count) / _packets_count;
        _alpha = (1 - _g) * _alpha + _g * F;


        // se ajusteaza cwd
        double new_cwd = _cwnd * (1 - _alpha / 2);
        _cwnd = max(new_cwd, _cwnd_floor);
        
        _next_decision = _highest_sent + _cwnd;

        // Reset counters
        _ecn_packets_count = 0;
        _packets_count = 0;
    } else {
        if (_cwnd < _ssthresh) {
            // slow start
            _cwnd += _mss;
        } else {
            // congestion avoidance
            _cwnd += (_mss * _mss* _cwnd_growth_factor) / _cwnd;
        }
    }
    
}

/* Receive function, depending on what is received it calls processLoss or processACK */
void CCSrc::receivePacket(Packet& pkt) {
    if (!_flow_started) {
        return;
    }

    switch (pkt.type()) {
    case CCNACK:
        processNack((const CCNack&)pkt);
        pkt.free();
        break;
    case CCACK:
        processAck((const CCAck&)pkt);
        pkt.free();
        break;
    default:
        cout << "Got packet with type " << pkt.type() << endl;
        abort();
    }

    // Now send packets!
    while (_flightsize + _mss < _cwnd)
        send_packet();
}

// Note: the data sequence number is the number of Byte1 of the packet, not the last byte.
/* Function called to transmit a packet */
void CCSrc::send_packet() {
    assert(_flow_started);

    CCPacket* p = CCPacket::newpkt(*_route, _flow, _highest_sent + 1, _mss, eventlist().now());

    _highest_sent += _mss;
    _packets_sent++;

    _flightsize += _mss;

    // cout << "Sent " << _highest_sent + 1 << " Flow Size: " << _flow_size << " Flow " << _name << " time " << timeAsUs(eventlist().now()) << endl;
    p->sendOn();
}

void CCSrc::doNextEvent() {
    if (!_flow_started) {
        startflow();
        return;
    }
}

////////////////////////////////////////////////////////////////
//  CC SINK Aici este codul ce ruleaza pe receptor, in mare nu o sa aducem multe modificari
////////////////////////////////////////////////////////////////

/* Only use this constructor when there is only one flow to this receiver */
CCSink::CCSink()
    : Logged("CCSINK"), _total_received(0)
{
    _src = 0;

    _nodename = "CCsink";
    _total_received = 0;
}

/* Connect a src to this sink. */
void CCSink::connect(CCSrc& src, Route* route)
{
    _src = &src;
    _route = route;
    setName(_src->_nodename);
}

// Receive a packet.
// seqno is the first byte of the new packet.
void CCSink::receivePacket(Packet& pkt) {
    switch (pkt.type()) {
    case CC:
        break;
    default:
        abort();
    }

    CCPacket *p = (CCPacket*)(&pkt);
    CCPacket::seq_t seqno = p->seqno();

    simtime_picosec ts = p->ts();
    // bool last_packet = ((CCPacket*)&pkt)->last_packet();

    if (pkt.header_only()) {
        send_nack(ts, seqno);
        p->free();

        // cout << "Wrong seqno received at CC SINK " << seqno << " expecting " << _cumulative_ack << endl;
        return;
    }

    int size = p->size() - ACKSIZE;
    _total_received += Packet::data_packet_size();

    bool ecn = (bool)(pkt.flags() & ECN_CE);

    send_ack(ts, seqno, ecn);
    // have we seen everything yet?
    pkt.free();
}

void CCSink::send_ack(simtime_picosec ts, CCPacket::seq_t ackno, bool ecn) {
    CCAck *ack = CCAck::newpkt(_src->_flow, *_route, ackno, ts, ecn);
    ack->sendOn();
}

void CCSink::send_nack(simtime_picosec ts, CCPacket::seq_t ackno) {
    CCNack *nack = CCNack::newpkt(_src->_flow, *_route, ackno, ts);
    nack->sendOn();
}
