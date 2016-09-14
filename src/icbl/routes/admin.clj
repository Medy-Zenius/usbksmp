(ns icbl.routes.admin
  (:require [compojure.core :refer :all]
            [icbl.views.layout :as layout]
            [noir.response :as resp]
            [noir.io :as io]
            [icbl.models.db :as db]
            [noir.session :as session]
            [clojure.data.json :as json]
            [icbl.routes.teacher :as teacher]
            ))

(defn num-to-str [number]
  (let [snum (str number)]
    (cond
      (<= (count snum) 2) (str snum ",00")
      (= (subs snum 1 2) ".") (if (= (count (subs snum 2 (count snum))) 1) (str (subs snum 0 1) "," (subs snum 2 3) "0")
                                  (str (subs snum 0 1) "," (subs snum 2 (count snum))))
      )))

(defn admin-home []
  (layout/render "admin/home.html")
  )

(defn handle-login [pass]
  (let [vpass (:pass (db/get-data (str "select pass from admin where id='admin'") 1))
        ip (:ipnumber (db/get-data "select ipnumber from ip where no=1" 1))]
    (if (= vpass pass)
        (do
          (session/put! :id "admin")
          (session/put! :status 1)
          (session/put! :ip ip)
          (layout/render "admin/work.html"))
        (layout/render "admin/home.html" {:error "Password Salah!"}))))

(defn handle-list-nama [nm]
  (let [upnm (clojure.string/upper-case nm)
        data (db/get-data (str "select nis,nama,kelas from users where upper(nama) LIKE '%" upnm "%' order by nama") 2)]
       (if data
         (layout/render "admin/list-siswa-nama.html" {:data data})
         (layout/render "admin/pesan.html" {:pesan "Tidak ada nama tersebut!"}))
    ))

(defn handle-do-edit-siswa [nis]
  (layout/render "admin/edit-data-siswa.html"
                 {:datum (db/get-data (str "select * from users where nis='" nis "'") 1)}))

(defn handle-update-data-siswa [nislama nisbaru nama kelas email pass]
  (try (db/update-data-1 "users"
                              ["nis=?" nislama]
                                      {:nis nisbaru
                                       :nama nama
                                       :kelas kelas
                                       :email email
                                       :password pass})
               (layout/render "admin/pesan.html" {:pesan "Berhasil mengubah data siswa!"})
               (catch Exception ex
                (layout/render "admin/pesan.html" {:pesan "Gagal mengubah data siswa!"}))))

(defn handle-ganti-pw-admin [pwlama pwbaru pwbaru1]
  (let [pwnow (:pass (db/get-data (str "select pass from admin where id='admin'") 1))]
    (if (or (not= pwlama pwnow) (< (count pwbaru) 5))
        (layout/render "admin/pesan.html" {:pesan "Password Lama tidak benar atau password baru kurang dari lima huruf!"})
        (if (= pwbaru pwbaru1)
          (try (spit "data/pw.txt" pwbaru)
                 (layout/render "admin/pesan.html" {:pesan "Berhasil mengubah password admin!"})
               (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan "Gagal mengubah data password admin!"})))
          (layout/render "admin/pesan.html" {:pesan "Gagal mengubah data password admin!"})))))

(defn lihat-guru []
  (let [data (db/get-data (str "select * from teacher order by nama asc") 2)]
    (layout/render "admin/lihat-guru.html" {:data data})))

(defn handle-edit-guru [id]
  (let [datum (db/get-data (str "select * from teacher where id='" id "'") 1)]
    (layout/render "admin/edit-guru.html" {:datum datum})))

(defn handle-update-guru [id nama pass]
  (try
    (db/update-data "teacher" (str "id='" id "'")
       {:nama nama
        :pass pass
        })
    (layout/render "admin/pesan.html" {:pesan (str "Berhasil Update Data Guru!")})
    (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan (str "Gagal Update Data Guru error: " ex)}))))

(defn daftarkan-guru []
  (layout/render "admin/daftarkan-guru.html"))

(defn handle-daftarkan-guru [id nama]
  (do
    (io/create-path (str "resources/public/proset/" id) true)
    (try
      (db/insert-data "teacher" {:nama nama :id id :pass "abcde"})
      (layout/render "admin/pesan.html" {:pesan (str "Berhasil daftarkan Bapak/Ibu " nama " dengan ID " id)})
      (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan (str "Gagal Daftarkan Guru error: " ex)})))))

(defn admin-pilih-guru [act]
  (let [data (db/get-data (str "select nama,id from teacher order by nama") 2)]
    (layout/render "admin/pilih-guru.html" {:action act :data data})))

(defn admin-set-ip []
  (let [ip (:ipnumber (db/get-data "select ipnumber from ip" 1))]
    (layout/render "admin/change-ip.html" {:ip ip})))

(defn admin-update-ip [ip]
  (try
      (db/update-data-1 "ip" ["no=?" 1] {:ipnumber ip})
      (layout/render "admin/pesan.html" {:pesan (str "Berhasil ubah IP Server menjadi " ip)})
      (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan (str "Gagal Ubah IP Server error: " ex)}))))

(defn logout []
  (do
   (session/clear!)
   (resp/redirect "/admin")))

(defn handle-admin-buat-proset [pel ket jsoal waktu]
  (try
      (db/insert-data "bankproset"
                               {:id (session/get :id)
                                :pelajaran pel
                                :keterangan ket
                                :jsoal (Integer/parseInt jsoal)
                                :waktu (Integer/parseInt waktu)
                                :kunci (apply str (repeat (Integer/parseInt jsoal) "-"))
                                :jenis (apply str (repeat (Integer/parseInt jsoal) "1"))
                                :upto (apply str (repeat (Integer/parseInt jsoal) "-"))
                                :acak "0"
                                :status "0"})
      (layout/render "admin/pesan.html" {:pesan (str "Berhasil daftarkan proset!")})
      (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan (str "Gagal daftarkan proset! error: " ex)}))))

(defn handle-admin-search-proset [pel ket act]
  (let [Uket (clojure.string/upper-case ket)
        data (db/get-data (str "select kode,pelajaran,keterangan,jsoal,waktu,status from bankproset where
                               pelajaran='" pel "' and upper(keterangan) LIKE '%" Uket "%'
                               order by keterangan") 2)]
    (layout/render "admin/list-proset.html" {:data data :action act :pel pel :ket ket})))

(defn admin-edit-proset [kode]
  (let [datum (db/get-data (str "select * from bankproset where kode='" kode "'") 1)]
    (layout/render "admin/edit-proset.html" {:datum datum})))

(defn admin-update-proset [kode pel ket jsoal waktu acak status]
  (let [datum (db/get-data (str "select kunci,jenis,upto from bankproset where kode='" kode "'") 1)
        oldkunci (datum :kunci)
        oldjenis (datum :jenis)
        oldupto (datum :upto)
        cok (count oldkunci)
        vjsoal (Integer/parseInt jsoal)
        newkunci (cond
                   (= vjsoal cok) oldkunci
                   (< vjsoal cok) (subs oldkunci 0 vjsoal)
                   :else (str oldkunci (apply str (repeat (- vjsoal cok) "-"))))
        newjenis (cond
                   (= vjsoal cok) oldjenis
                   (< vjsoal cok) (subs oldjenis 0 vjsoal)
                   :else (str oldjenis (apply str (repeat (- vjsoal cok) "1"))))
        newupto (cond
                   (= vjsoal cok) oldupto
                   (< vjsoal cok) (subs oldupto 0 vjsoal)
                   :else (str oldupto (apply str (repeat (- vjsoal cok) "-"))))]
  (try
    (db/update-data "bankproset" (str "kode='" kode "'")
                    {:pelajaran pel :keterangan ket
                     :jsoal vjsoal
                     :waktu (Integer/parseInt waktu)
                     :acak acak
                     :status status
                     :kunci newkunci
                     :jenis newjenis
                     :upto newupto})
    (layout/render "admin/pesan.html" {:pesan (str "Berhasil update proset!")})
    (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan (str "Gagal update proset! error: " ex)})))))

(defn admin-upload-file [kode pel]
  (do
    (io/create-path (str "resources/public/bankproset/" pel "/" kode) true)
    (layout/render "admin/upload.html" {:kode kode :pel pel})))

(defn handle-admin-upload [pel kode file]
  (do
    (io/upload-file (str "resources/public/bankproset/" pel "/" kode) file)
    (layout/render "admin/upload.html" {:kode kode :pel pel})))

(defn admin-edit-kunci [kode]
  (let [datum (db/get-data (str "select kunci,jsoal,jenis,upto from bankproset where kode='" kode"'") 1)]
    (layout/render "admin/edit-kunci.html" {:kunci (datum :kunci)
                                              :jsoal (datum :jsoal)
                                              :jenis (datum :jenis)
                                              :upto (datum :upto)
                                              :kode kode})))

(defn admin-save-kunci [kunci jenis upto kode]
  (try
    (db/update-data "bankproset" (str "kode='" kode "'") {:kunci kunci :jenis jenis :upto upto})
    (layout/render "admin/pesan.html" {:pesan "Kunci berhasil disimpan!"})
    (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan (str "Gagal simpan kunci! error: " ex)}))))

(defn admin-view-soal [pel kode]
  (let [datum (db/get-data (str "select * from bankproset where kode='" kode "'") 1)]
    (layout/render "admin/view-soal.html" {:datum datum
                                             :nsoal (vec (range 1 (inc (datum :jsoal))))
                                             :kategori "1"
                                             :pel pel
                                             ;:soalpath "http://127.0.0.1/resources/public"
                                             })))

(defn admin-lihat-sekaligus [pel kode]
  (let [datum (db/get-data (str "select * from bankproset where kode='" kode "'") 1)]
    (layout/render "admin/view-soal-sekaligus.html" {:datum datum
                                                       :pel pel
                                                       ;soalpath "http://localhost/resources/public"
                                                       })))

(defn admin-hapus-set [pel ket kode]
  (try
    (db/delete-data "bankproset" (str "kode='" kode "'"))
    (handle-admin-search-proset pel ket "/admin-hapus-set1")
    (catch Exception ex
      (layout/render "teacher/pesan.html" {:pesan (str "Gagal Hapus Proset! error " ex)}))
    ))

;;;routes
(defroutes admin-routes

  (GET "/admin" []
      (admin-home))

  (GET "/admin-home" []
       (layout/render "admin/work.html"))

  (GET "/admin-logout" []
       (logout))

  (POST "/admin-login" [pass]
      (handle-login pass))

  (GET "/edit-siswa" []
       (layout/render "admin/search-siswa.html"))
  (POST "/edit-siswa" [nama]
        (handle-list-nama nama))
  (POST "/do-edit-siswa" [nis]
        (handle-do-edit-siswa nis))
  (POST "/update-data-siswa" [nislama nisbaru nama kelas email pass]
        (handle-update-data-siswa nislama nisbaru nama kelas email pass))

  (GET "/ganti-pw-admin" []
       (layout/render "admin/ganti-pw-admin.html"))
  (POST "/ganti-pw-admin" [pwlama pwbaru pwbaru1]
        (handle-ganti-pw-admin pwlama pwbaru pwbaru1))

  (GET "/lihat-guru" []
       (lihat-guru))
  (POST "/edit-guru" [id]
        (handle-edit-guru id))
  (POST "/update-guru" [id nama pass]
        (handle-update-guru id nama pass))

  (GET "/daftarkan-guru" []
       (daftarkan-guru))
  (POST "/daftarkan-guru" [id nama]
        (handle-daftarkan-guru id nama))

  (GET "/admin-hasil-testL" []
       (admin-pilih-guru "/admin-pilih-proset"))
  (POST "/admin-pilih-proset" [id]
        (teacher/teacher-pilih-proset "L" id "/teacher-hasil-test"))
  (GET "/admin-hasil-testB" []
       (layout/render "admin/search-proset.html" {:act "/admin-hasil-test-search"}))
  (POST "/admin-hasil-test-search" [pel ket]
       (handle-admin-search-proset pel ket "/admin-hasil-testB"))
  (POST "/admin-hasil-testB" [kode]
       (teacher/teacher-hasil-test kode "teacher/hasil-test.html"))

  ;;Analisis Butir Soal
  (GET "/admin-abs" []
        (admin-pilih-guru "/admin-pilih-proset-absbsk"))
  (POST "/admin-pilih-proset-absbsk" [id]
        (teacher/teacher-pilih-proset id "/teacher-abs"))

  (GET "/admin-abs-tk" []
       (admin-pilih-guru "/admin-pilih-proset-abstk"))
  (POST "/admin-pilih-proset-abstk" [id]
       (teacher/teacher-pilih-proset id "/teacher-abs-tk"))

  (GET "/admin-abs-dp" []
       (admin-pilih-guru "/admin-pilih-proset-absdp"))
  (POST "/admin-pilih-proset-absdp" [id]
       (teacher/teacher-pilih-proset id "/teacher-abs-dp"))

  (GET "/admin-dayakecoh" []
       (admin-pilih-guru "/admin-pilih-proset-absdk"))
   (POST "/admin-pilih-proset-absdk" [id]
       (teacher/teacher-pilih-proset id "/teacher-dayakecoh"))

  ;;Simpan ke Excel

  (GET "/admin-hasil-test-excel" []
       (admin-pilih-guru "/admin-pilih-proset-excel"))
  (POST "/admin-pilih-proset-excel" [id]
       (teacher/teacher-pilih-proset id "/teacher-hasil-test-excel"))

  (GET "/admin-abs-excel" []
        (admin-pilih-guru "/admin-pilih-proset-abs-excel"))
   (POST "/admin-pilih-proset-abs-excel" [id]
       (teacher/teacher-pilih-proset id "/teacher-abs-excel"))

  (GET "/admin-abs-tk-excel" []
        (admin-pilih-guru "/admin-pilih-proset-abstk-excel"))
   (POST "/admin-pilih-proset-abstk-excel" [id]
       (teacher/teacher-pilih-proset id "/teacher-abs-tk-excel"))

  (GET "/admin-abs-dp-excel" []
        (admin-pilih-guru "/admin-pilih-proset-absdp-excel"))
   (POST "/admin-pilih-proset-absdp-excel" [id]
       (teacher/teacher-pilih-proset id "/teacher-abs-dp-excel"))

  (GET "/admin-adk-excel" []
        (admin-pilih-guru "/admin-pilih-proset-adk-excel"))
   (POST "/admin-pilih-proset-adk-excel" [id]
       (teacher/teacher-pilih-proset id "/teacher-adk-excel"))

  (GET "/admin-set-ip" []
       (admin-set-ip))
  (POST "/admin-change-ip" [ipnumber]
        (admin-update-ip ipnumber))

  (GET "/admin-buat-proset" []
       (layout/render "admin/buat-proset.html"))
  (POST "/admin-buat-proset" [pel ket jsoal waktu]
        (handle-admin-buat-proset pel ket jsoal waktu))

  (GET "/admin-search-proset" []
       (layout/render "admin/search-proset.html" {:act "/admin-search-proset"}))
  (POST "/admin-search-proset" [pel ket]
        (handle-admin-search-proset pel ket "/admin-edit-proset"))
  (POST "/admin-edit-proset" [kode]
        (admin-edit-proset (subs kode 1 (count kode))))
  (POST "/admin-update-proset" [kode pel ket jsoal waktu acak status]
         (admin-update-proset kode pel ket jsoal waktu acak status))

  (GET "/admin-upload-file" []
       (layout/render "admin/search-proset.html" {:act "/admin-pilih-proset1"}))
  (POST "/admin-pilih-proset1" [pel ket]
       (handle-admin-search-proset pel ket "/admin-upload-file1"))
  (POST "/admin-upload-file1" [kode pel]
        (admin-upload-file (subs kode 1 (count kode)) pel))
  (POST "/admin-upload" [pel kode file]
        (handle-admin-upload pel kode file))

  (GET "/admin-edit-kunci" []
       (layout/render "admin/search-proset.html" {:act "/admin-edit-kunci-search"}))
  (POST "/admin-edit-kunci-search" [pel ket]
      (handle-admin-search-proset pel ket "/admin-edit-kunci1"))
  (POST "/admin-edit-kunci1" [kode]
        (admin-edit-kunci (subs kode 1 (count kode))))
  (POST "/admin-save-kunci" [kunci jenis upto kode]
        (admin-save-kunci kunci jenis upto kode))

  (GET "/admin-lihat-soal" []
       (layout/render "admin/search-proset.html" {:act "/admin-lihat-soal-search"}))
  (POST "/admin-lihat-soal-search" [pel ket]
      (handle-admin-search-proset pel ket "/admin-lihat-soal1"))
  (POST "/admin-lihat-soal1" [pel kode]
        (admin-view-soal pel (subs kode 1 (count kode))))

  (GET "/admin-lihat-sekaligus" []
       (layout/render "admin/search-proset.html" {:act "/admin-lihat-sekaligus-search"}))
  (POST "/admin-lihat-sekaligus-search" [pel ket]
      (handle-admin-search-proset pel ket "/admin-lihat-sekaligus1"))
  (POST "/admin-lihat-sekaligus1" [pel kode]
        (admin-lihat-sekaligus pel (subs kode 1 (count kode))))

  (GET "/admin-hapus-set" []
       (layout/render "admin/search-proset.html" {:act "/admin-hapus-set-search"}))
  (POST "/admin-hapus-set-search" [pel ket]
      (handle-admin-search-proset pel ket "/admin-hapus-set1"))
  (POST "/admin-hapus-set1" [pel ket kode]
        (admin-hapus-set pel ket (subs kode 1 (count kode))))
)
