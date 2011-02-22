(ns visugit.shell-out
  (:require [clojure.java.io :as jio :only [file]])
  (:import [java.io ByteArrayOutputStream
            ByteArrayInputStream
            InputStream OutputStream]
           [org.apache.commons.exec
            CommandLine
            DefaultExecutor
            DefaultExecuteResultHandler
            ExecuteException
            ExecuteResultHandler
            ExecuteWatchdog
            PumpStreamHandler
            ShutdownHookProcessDestroyer
            Watchdog]
           [org.apache.commons.exec.environment EnvironmentUtils]))

(defn parse-args [args]
  (split-with string? args))

(defn- all-close [^InputStream in,
                  ^OutputStream out,
                  ^OutputStream err, opts]
  (when (and in (not (:no-close-in opts)))
    (.close in))
  (when (not (:no-close-out opts))
    (.close out))
  (when (not (:no-close-err opts))
    (.close err)))

(defn- conv [st ^String enc]
  (when (instance? ByteArrayOutputStream st)
    (let [b (.toByteArray ^ByteArrayOutputStream st)]
      (cond (nil? (seq b)) nil
            (= enc :byte)  b
            (string? enc)  (String. b enc)
            :else (String. b (System/getProperty "file.encoding"))))))

(defn response [in out err opts exit-value]
  (let [enc (:encode opts)
        o (conv out enc)
        e (conv err enc)]
    (all-close in out err opts)
    {:exit exit-value,
     :out o,
     :err e}))
  
(defrecord MyResultHandler [ret in out err opts]
  ExecuteResultHandler
  (onProcessComplete
   [_ exit-value]
   (deliver ret (response in out err opts exit-value)))
  (onProcessFailed
   [_ e]
   (deliver ret
            (assoc (response in out err opts (.getExitValue e))
              :fail e))))

(defn string->input-stream [^String s ^String encode]
  (ByteArrayInputStream. (.getBytes s (or encode (System/getProperty "file.encoding")))))

(defn sh [& args-and-opts]
  (let [[[^String comm & args] [opts]] (parse-args args-and-opts)
        command (CommandLine. comm)
        in  (when-let [i (:in opts)]
              (if (string? i) (string->input-stream i (:encode opts)) i))
        out (or (:out opts) (ByteArrayOutputStream.))
        err (or (:err opts) (ByteArrayOutputStream.))
        stream-handler (PumpStreamHandler. out err in)
        executor (DefaultExecutor.)
        ret (promise)
        ^ExecuteResultHandler result-handler (MyResultHandler. ret in out err opts)]
    (doseq [arg args]
      (.addArgument command arg))
    (when-let [dir (:dir opts)]
      (.setWorkingDirectory executor (jio/file dir)))
    (when-let [success (:as-success opts)]
      (.setExitValue executor success))
    (when-let [successes (:as-successes opts)]
      (.setExitValues executor (into-array Integer/TYPE successes)))
    (when-let [ms (:watchdog opts)]
      (.setWatchdog executor (ExecuteWatchdog. ms)))
    (when-not (:no-destroy opts)
      (.setProcessDestroyer executor (ShutdownHookProcessDestroyer.)))
    (.setStreamHandler executor stream-handler)
    (if-let [env (:env opts)]
      (.execute executor command env result-handler)
      (if-let [add-env (:add-env opts)]
        (let [env (EnvironmentUtils/getProcEnvironment)]
          (doseq [[k v] add-env]
            (.put env k v))
          (.execute executor command env result-handler))
        (.execute executor command result-handler)))
    ret))
