//
//  SwiftHaskellApp.swift
//  SwiftHaskell
//
//  Created by Rodrigo Mesquita on 09/11/2023.
//

import SwiftUI
import HaskellFramework.RTSManage

@main
struct SwiftHaskellApp: App {
    init() {
        flib_init()

        NotificationCenter.default.addObserver(forName: NSApplication.willTerminateNotification, object: nil, queue: .main) { _ in
            // terminating
            flib_end()
        }
    }
    var body: some Scene {
        WindowGroup {
            ContentView()
        }
    }
}
