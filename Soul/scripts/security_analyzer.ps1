# Security Incident Response Analyzer
# Investigate potential hack/compromise

Write-Host "=== SECURITY INCIDENT RESPONSE ===" -ForegroundColor Red
Write-Host "Investigating potential system compromise..." -ForegroundColor Yellow

# Check for suspicious network connections
Write-Host "`n[1] Checking Active Network Connections:" -ForegroundColor Cyan
try {
    $connections = Get-NetTCPConnection | Where-Object {$_.State -eq "Established"} | 
                   Select-Object LocalAddress, LocalPort, RemoteAddress, RemotePort, OwningProcess
    $connections | ForEach-Object {
        $processName = (Get-Process -Id $_.OwningProcess -ErrorAction SilentlyContinue).ProcessName
        Write-Host "  $($_.RemoteAddress):$($_.RemotePort) <- $processName" -ForegroundColor White
    }
} catch {
    Write-Host "  Error checking connections: $($_.Exception.Message)" -ForegroundColor Red
}

# Check for suspicious processes
Write-Host "`n[2] Checking Running Processes:" -ForegroundColor Cyan
try {
    $suspiciousProcesses = Get-Process | Where-Object {
        $_.ProcessName -match "(hack|remote|vnc|teamviewer|anydesk)" -or
        $_.CPU -gt 50
    } | Select-Object ProcessName, Id, CPU, WorkingSet
    
    if ($suspiciousProcesses) {
        $suspiciousProcesses | ForEach-Object {
            Write-Host "  SUSPICIOUS: $($_.ProcessName) (PID: $($_.Id)) CPU: $($_.CPU)" -ForegroundColor Red
        }
    } else {
        Write-Host "  No obviously suspicious processes detected" -ForegroundColor Green
    }
} catch {
    Write-Host "  Error checking processes: $($_.Exception.Message)" -ForegroundColor Red
}

# Check recent logon events
Write-Host "`n[3] Checking Recent Logon Events:" -ForegroundColor Cyan
try {
    $recentLogons = Get-WinEvent -FilterHashtable @{LogName='Security'; ID=4624} -MaxEvents 10 -ErrorAction SilentlyContinue |
                    Select-Object TimeCreated, Id, @{Name='Message';Expression={$_.Message.Split("`n")[0]}}
    
    $recentLogons | ForEach-Object {
        Write-Host "  $($_.TimeCreated): $($_.Message)" -ForegroundColor Yellow
    }
} catch {
    Write-Host "  Unable to access security logs (need admin rights)" -ForegroundColor Yellow
}

# Check for IoT/Smart Home devices on network
Write-Host "`n[4] Scanning for Smart Home Devices:" -ForegroundColor Cyan
try {
    $localIP = (Get-NetIPAddress -AddressFamily IPv4 | Where-Object {$_.IPAddress -like "192.168.*" -or $_.IPAddress -like "10.*"}).IPAddress[0]
    $subnet = $localIP.Substring(0, $localIP.LastIndexOf('.'))
    
    Write-Host "  Scanning subnet: $subnet.x" -ForegroundColor White
    
    # Quick ping sweep for common IoT device IPs
    $deviceIPs = @("$subnet.1", "$subnet.100", "$subnet.101", "$subnet.200", "$subnet.254")
    foreach ($ip in $deviceIPs) {
        if (Test-Connection -ComputerName $ip -Count 1 -Quiet -TimeoutSeconds 2) {
            Write-Host "  ACTIVE DEVICE: $ip" -ForegroundColor Yellow
        }
    }
} catch {
    Write-Host "  Error scanning network: $($_.Exception.Message)" -ForegroundColor Red
}

# Check Windows Defender status
Write-Host "`n[5] Checking Windows Defender Status:" -ForegroundColor Cyan
try {
    $defenderStatus = Get-MpComputerStatus
    Write-Host "  Real-time Protection: $($defenderStatus.RealTimeProtectionEnabled)" -ForegroundColor White
    Write-Host "  Last Quick Scan: $($defenderStatus.QuickScanStartTime)" -ForegroundColor White
    Write-Host "  Threats Found: $($defenderStatus.ThreatName)" -ForegroundColor White
} catch {
    Write-Host "  Error checking Defender: $($_.Exception.Message)" -ForegroundColor Red
}

Write-Host "`n=== IMMEDIATE ACTIONS RECOMMENDED ===" -ForegroundColor Red
Write-Host "1. Disconnect from internet if still compromised" -ForegroundColor Yellow
Write-Host "2. Change all passwords from a clean device" -ForegroundColor Yellow
Write-Host "3. Check smart home device admin panels" -ForegroundColor Yellow
Write-Host "4. Run full antivirus scan" -ForegroundColor Yellow
Write-Host "5. Contact smart lock manufacturer support" -ForegroundColor Yellow

Write-Host "`nSecurity analysis complete!" -ForegroundColor Green
