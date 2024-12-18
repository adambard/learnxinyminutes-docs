---
name: Nmap
category: tool
contributors:
    - ["Sebastian Oberdorfer", "https://github.com/SOberdorfer"]
---

### Learn Nmap in Y Minutes

So, you’re connected to a network and want to know what else is connected to it.
Maybe you’re trying to find that mystery device eating up bandwidth or check
if there are services running you didn’t know about, or you just want to verify
what ports are exposed on your machine?

Meet your swiss-army network knife named **Nmap**!

---

### Introduction

**Nmap 101**  
Nmap is an open-source network scanning tool built by Gordon Lyon. Designed to
help you find devices, open ports and services across your network.
It’s a swiss-army knife for network admins, security folks, dev's and anyone
curious about what’s living on their network.

**When to Use It**

- **Finding Devices**: What’s connected, and what’s running?
- **Network Troubleshooting**: Resolve DNS or connection issues.
- **Vulnerability Detection**: Spotting potentially risky services.
- **Network Security**: Evaluate exposed ports.

**When *Not* to Use It**

- **Public Networks**: Scanning Starbucks WiFi might land you in hot tea.
- **Corporate Networks**: Scanning your corporate network without permission, is
  potentially not allowed.
- **Global Web**: In some cases scanning across the web can be illegal.

Certain scans are intrusive and can trigger security alarms, so stick to **only
**
scanning networks or systems where you have permission. Unauthorized scanning
can be considered illegal under cybersecurity laws in many regions, and
companies
might view it as a hacking attempt.

Use Nmap extensively and wisely.

---

### Installation

Installation is straightforward, thoroughly explained on [nmap.org - install](https://nmap.org/book/install.html)

---

### The Basics

These are low-key scans that safe to use since they don’t do deep probing.

- **Ping Scan**:  
  A low-impact scan just to check if devices are online. Typically fine on
  trusted networks.
    - Scan a single device
      ```bash
       nmap -sn 192.168.1.1
      ```
    - Scan a range of devices
      ```bash
      nmap -sn 192.168.1.1-100
      ```
    - Scan a CIDR range of devices
      ```bash
      nmap -sn 192.168.1.0/24   # Range 192.168.1.0 to 192.168.1.255
      nmap -sn 192.168.0.0/16   # Range 192.168.0.0 to 192.168.255.255
      nmap -sn 192.0.0.0/8      # Range 192.0.0.0 to 192.255.255.255
      ```

- **Fast Scan**:  
  Quickly checks the 100 most common ports. Great for a quick peek without
  probing all 65,535 ports.
  ```bash
  nmap -F 192.168.1.1
  ```

- **Operating System Detection**:  
  OS detection requires some extra probing, which might be detectable by
  Intrusion Detection Systems (IDS).
  ```bash
  nmap -O 192.168.1.1
  ```

- **Output to File**  
  Specific scanning and saving the output to a file, enables you to scan more
  thorough without overloading your network.
    - Plain text
      ```bash
      nmap -oN output.txt 192.168.1.1 
      ```
    - XML, handy for using elsewhere
      ```bash
      nmap -oX output.xml 192.168.1.1
      ```

---

### Moving Up: More Insightful Scans

These scans dig a bit deeper, so they may trigger alarms on security systems.
Use these only on networks where you have explicit permission to scan.

- **Service Version Detection**:  
  Tries to identify versions of services on open ports. Useful but more
  invasive.
  ```bash
  nmap -sV 192.168.1.1
  ```

- **Aggressive Scan**:  
  The aggressive scan mode (`-A`) combines multiple checks, like OS detection,
  version detection and traceroute. This is likely to be flagged on
  any network and can be considered illegal on networks you don’t own.
  ```bash
  nmap -A 192.168.1.1
  ```

- **Scanning Specific Ports**:  
  Narrowing scans to specific ports is generally fine.
    - Scan a specific port
      ```bash
      nmap -p 80 192.168.1.1
      ```
    - Scan a range of ports
      ```bash
      nmap -p 1-100 192.168.1.1
      ```

---

### Advanced Scans: When You’re the Power User

So, you’re getting into the advanced stuff—maybe testing your own firewall or
finding rogue services.
The following scans are loud and intrusive that definitely trigger security
defenses.

- **Scripted Scans (NSE)**  
  Nmap’s script engine is like a toolbox of plugins. Need to check for a
  specific vulnerability? There’s likely an NSE script for it.
  ```bash
  nmap --script=http-vuln-cve2021-12345 192.168.1.1
  ```

- **Aggressive and fastest Scans**:
  `-T5` turns up to knob to 11. `-A` scans all ports.
  Use it sparse and only if you really need full visibility.
  ```bash
  nmap -T5 -A 192.168.1.1
  ```

- **TCP and UDP Combined Scans**:  
  Combining TCP and UDP scans (`-sS` for SYN scans and `-sU` for UDP) gives
  complete coverage but increases the scan’s footprint, making it detectable.
  ```bash
  nmap -sS -sU 192.168.1.1
  ```

- **Spoofing and Decoy Scans**:  
  Using decoys (`-D`) or spoofed IP addresses to hide your real IP can be seen
  as deceptive. These scans are easily flagged by IDS and could lead to legal
  repercussions if you’re not authorized.
  ```bash
  # 10 random IP decoys
  nmap -D RND:10 192.168.1.1 
  ```

---

### Practical Tips and Tricks

**Timing Templates**  
Nmap has timing options from `-T0` (paranoid) to `-T5` (insane). Stick with
`-T2` or `-T3` for a good balance between speed and not making too much noise.
More
on [nmap - timing-templates](https://nmap.org/book/performance-timing-templates.html)

**Check Out Nmap’s Scripts**  
NSE scripts make Nmap super versatile. From DNS enumeration to vulnerability
checks, there’s probably a script for whatever you need.
More on [nmap - Nmap Scripting Engine](https://nmap.org/book/man-nse.html)

**Use aggressive scans and decoys only on networks you own** or with formal
authorization, such as during a penetration test with client permission. If
you’re running scans at work, talk to the network admins first.

**Know When to Stop**  
Once you’ve got the info you need, wrap it up. It’s easy to get scan-happy.

---

Happy scanning!
