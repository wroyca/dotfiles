#! /usr/bin/env bash

# Kernel preemption is a property possessed by some kernels, in which the CPU
# can be interrupted in the middle of executing kernel code and assigned other
# tasks (from which it later returns to finish).
#
# This allows applications to run smoothly even when the system is under load,
# at the cost of slighly lower throughput and a slight runtime overhead to
# kernel code.
#
# See also
# https://www.spinics.net/lists/fedora-devel/msg314320.html
#
sudo grubby --args="preempt=full" --update-kernel=ALL
